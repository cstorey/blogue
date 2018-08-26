# 


.PHONY: nodrafts rebuild clean clean-wp clean-site site-build site-rebuild prettier

SETUP=.done.setup
YARN_INSTALL=.done.npm-install

STACK_BUILD = .done.stack-build
YARN_BUILD = out/manifest.json

all: site-build

clean: clean-wp clean-flags clean-site
clean-flags:
	rm -f $(SETUP) $(YARN_INSTALL)

clean-wp:
	rm -rf out

clean-site:
	rm -rf _site _cache

nodrafts: 
	find posts -type l -exec rm -vf {} \+

$(SETUP):
	stack setup
	touch $@

$(YARN_INSTALL): package.json
	yarn install
	touch $@

$(YARN_BUILD): $(YARN_INSTALL) $(wildcard css/*)
	yarn run build
	touch $@

$(STACK_BUILD): package.yaml stack.yaml site.hs $(wildcard src/*.hs)
	stack build 
	touch $@

site-build: $(SETUP) $(YARN_INSTALL) $(STACK_BUILD) $(YARN_BUILD) out/manifest.json posts/*.md
	stack exec -- site build
site-rebuild: $(SETUP) $(YARN_INSTALL) $(STACK_BUILD) $(YARN_BUILD) out/manifest.json posts/*.md
	stack exec -- site rebuild

watchexec-%:
	watchexec -- $(MAKE) $*

serve: $(STACK_BUILD)
	stack exec -- site server --host 0.0.0.0


prettier:
	yarn run prettier
