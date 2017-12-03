---
date: '2017-10-23'
orig_url: null
title: "Local build orchestration with make(1)"
description: 'Orchestrating the orchestrators'
---

Over the past few weeks I've been focusing mostly on build and deployment tooling around docker and Kubernetes. One particular downside of the current system, is our applications have a fair number of service dependencies.<!--more--> Up until now, we've taken to running everything inside docker using [`docker-compose`](https://docs.docker.com/compose/), but this feels to me more like a way to sweep complexities and technical problems under the rug, so to speak.

While there is some value in keeping all of your automation hacks in one place, it can make life difficult for anyone who doesn't use precisely that workflow (eg: developers who need to be able to use live-reloading, for one), or people who need ready access to debugging tools such as `strace(1)` and the like.

Other issues include that different environments, such as a developer workstation and testing on a build server have different needs. For one, if your builds run inside a docker container, and your dev workflow requires that you mount volumes into a container, then because docker assumes that any volume paths are relative to the host, not the context that `docker-compose` runs in, it's more than likely you'll end up with a build build failures because docker seems to (by default) create an empty `root`-owned directory on the host where it doesn't already exist. So, if you expect artifacts such as HTML templates or Javascript to be present, you'll be sadly disapointed.

So, in this spirit of bringing our dev environments closer to what we run in production, I've started building out some infrastructure to run and test our applications in [minikube](https://kubernetes.io/docs/getting-started-guides/minikube/). Being that Kubernetes etc. are somewhat fashionable, we're going to use that wonderful tool from the 1970's called [`make(1)`](https://en.wikipedia.org/wiki/Make_(software)) to do most of the heavy lifting.

The crux of our developer workflow is the build-test-reload cycle, and most of our developers (simplified for diadactic purposes) will be using a clojure repl. 

So, the makefile below ensures when you run `make repl` it'll:

 * build any images required for dependencies (elided here, but identical to that for the application image),
 * publish them,
 * re-configure kubenetes
 * Start a leiningen repl configured with addresses of the exposed services.

We run the repl on the host workstation directly, because whilst it's possible to export a host directory into minikube and use it within a kubenetes container; it's not nearly as simple and requires some degree of indirection.

Alternatively, You can use just `make deploy` to push the whole lot into kubernetes, and test as you like in a browser.

There's a few interesting patterns here worth mentioning, though.

Image tag versioning:

We automatically compute the tag name from the current git version, and developer identity. On the build server, we'll override this with just the git hash by using something akin to `make IMAGE_TAG=$(git log -n 1 --pretty=format:'%h')`. 

Stamp files:

Because docker images aren't concrete in the traditional unix sense (ie: they're not just a simple file); we use a `.stamp` file as a proxy for the image being built, and published. Where the image tag may vary, we include `$(IMAGE_TAG)` in the stamp file name to ensure that whenever the image tag changes, we re-do work where required.

```make
WORKDIR = work
WORKDIR_EXISTS = $(WORKDIR)/.exists

K8S_DEPS_SRC := k8s/pg-core.yaml
K8S_APP_SRC  :=  k8s/app-core.yaml

K8S_NAMESPACE = localdev
K8S_CONTEXT = minikube
K8S_ZONE = k8s.local
REGISTRY = my-lovely-registry

KUBECTL = kubectl --context $(K8S_CONTEXT) -n $(K8S_NAMESPACE)

# By default, we want to make it obvious who has built an image and where it is from.
# We can override this when building releases on the build server.
IMAGE_TAG := $(shell echo dev-$$(whoami)-$$(git log -n 1 --pretty=format:'%h') )

.PHONY: start clean total-destroy deploy deploy-dependencies \
	deploy-application purge images push-images \
	app-jar repl

all: deploy

clean:
	rm -rf $(WORKDIR)

total-destroy: clean
	rm -rf target

cluster-status:
	minikube status

# We don't just specify $(WORKDIR), as it's modification time will change
# every time a new file is created, and so anything depending on it directly
# will be re-built needlessly.
$(WORKDIR)/.exists:
	mkdir -vp $(WORKDIR)
	touch $@

jar: target/app.jar
target/app.jar: $(shell find src -type f -print)
	./lein uberjar

$(WORKDIR)/image-app-core-build.$(IMAGE_TAG).stamp: $(WORKDIR_EXISTS) target/app.jar
	docker build -t app/app-core:$(IMAGE_TAG) .
	touch $@

# Convenience alias for the images.
images: $(WORKDIR)/image-app-core-build.$(IMAGE_TAG).stamp

$(WORKDIR)/image-app-core-push.$(IMAGE_TAG).stamp: $(WORKDIR)/image-app-core-build.stamp
	docker tag app/app-core:$(IMAGE_TAG) $(REGISTRY)/app/app-core:$(IMAGE_TAG)
	docker push $(REGISTRY)/app/app-core:$(IMAGE_TAG)
	touch $@

# Convenience alias to publish images.
push-images: $(WORKDIR)/image-app-core-push.stamp $(WORKDIR)/image-hydra-setup-core-push.stamp

# Build the kubernetes description of dependencies, substituting our current
# version for `IMAGE_TAG`.
$(WORKDIR)/dependencies.yaml: $(VERSION_STAMP) $(K8S_DEPS_SRC)
	# Concatenate all files.
	tmpf=$$(mktemp -d $(WORKDIR)/deps-XXXXXX) && \
	for f in $(K8S_DEPS_SRC); do \
		echo "---";
		sed -e "s/IMAGE_TAG/$(IMAGE_TAG)/g" "$$f";
	done > $$tmpf && mv -v $$tmpf $@

# Build the kubernetes description of the application, substituting our
# current version for `IMAGE_TAG`.
$(WORKDIR)/application.yaml: $(VERSION_STAMP) $(K8S_APP_SRC)
	# Concatenate all files.
	tmpf=$$(mktemp -d $(WORKDIR)/deps-XXXXXX) && \
	for f in $(K8S_APP_SRC); do \
		echo "---";
		sed -e "s/IMAGE_TAG/$(IMAGE_TAG)/g" "$$f";
	done > $$tmpf && mv -v $$tmpf $@

# Create the namespace, if required.
$(WORKDIR)/k8s-ns-$(K8S_NAMESPACE).stamp:
	if ! $(KUBECTL) get namespace/$(K8S_NAMESPACE); then \
		$(KUBECTL) create namespace $(K8S_NAMESPACE); \
	fi
	touch $@

# Once we have all of our pre-requisites assembled, deploy our dependencies.
deploy-dependencies: $(WORKDIR)/dependencies.yaml \
		$(WORKDIR)/k8s-ns-$(K8S_NAMESPACE).stamp \
		$(WORKDIR)/image-app-core-push.$(IMAGE_TAG).stamp
	$(KUBECTL) apply -f $<

# Similarly for the application itself.
deploy-application: $(WORKDIR)/application.yaml deploy-dependencies
	$(KUBECTL) apply -f $<

deploy: deploy-dependencies deploy-application

# Here we look up the IP of the minikube vm, port numbers of any NodePort
# services (for dependencies like Postgres), and passes them to the
# application as environment variables.
repl: deploy-dependencies
	ip=$$(minikube ip) && \
	pg_port=$$(kubectl --context=minikube -n localdev get svc/pg-core -o \
	  go-template='{{range .spec.ports}}{{if .nodePort}}{{.nodePort}}{{"\n"}}{{end}}{{end}}'
	) && \
	POSTGRES=jdbc://$${ip}:$${pg_port}/my-db ./lein repl
```

