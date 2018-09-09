# 


.PHONY: nodrafts rebuild clean clean-wp clean-site site-build site-rebuild prettier

SETUP=.done.setup
YARN_INSTALL= .done.npm-install

STACK_BUILD = .done.stack-build
YARN_BUILD = .done.yarn-build
PY_SETUP = .done.py-setup

PY_SVG_GEN = $(wildcard images/*/*.svg.py)
PY_SVGS = $(patsubst %.svg.py,%.svg,$(PY_SVG_GEN))

PY_VENV = ./.venv
PYTHON = $(PY_VENV)/bin/python

all: site-build

clean: clean-wp clean-flags clean-site clean-gen
clean-flags:
	rm -f $(SETUP) $(YARN_INSTALL)

clean-wp:
	rm -rf out

clean-site:
	rm -rf _site _cache
clean-gen:
	rm -rf $(PY_SVGS)

nodrafts: 
	find posts -type l -exec rm -vf {} \+

$(SETUP):
	stack setup
	touch $@

$(YARN_INSTALL): package.json
	yarn install
	touch $@

$(YARN_BUILD): $(YARN_INSTALL) webpack.config.js postcss.config.js $(wildcard css/*)
	yarn run build
	touch $@

$(STACK_BUILD): package.yaml stack.yaml site.hs $(wildcard src/*.hs)
	stack build 
	touch $@

$(PYTHON):
	virtualenv -p python2 $(PY_VENV)

$(PY_SETUP): $(PYTHON) requirements.txt
	$(PY_VENV)/bin/pip install -r requirements.txt
	touch $@

$(PY_SVGS): %.svg : %.svg.py $(PY_SETUP)
	$(PYTHON) $< > /tmp/make-$$$$ && mv -v /tmp/make-$$$$ $@

site-build: $(SETUP) $(YARN_INSTALL) $(STACK_BUILD) $(YARN_BUILD) out/manifest.json posts/*.md $(PY_SVGS)
	stack exec -- site build
site-rebuild: $(SETUP) $(YARN_INSTALL) $(STACK_BUILD) $(YARN_BUILD) out/manifest.json posts/*.md $(PY_SVGS)
	stack exec -- site rebuild

watchexec-%:
	watchexec -- $(MAKE) $*

serve: $(STACK_BUILD)
	stack exec -- site server --host 0.0.0.0


prettier:
	yarn run prettier

