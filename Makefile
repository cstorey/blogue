# 


.PHONY: nodrafts rebuild clean clean-wp clean-site site-build site-rebuild

SETUP=.done.setup
NPM_INSTALL=.done.npm-install

NPM_BUILD = out/manifest.json

all: site-build

clean: clean-wp clean-flags clean-site
clean-flags:
	rm -f $(SETUP) $(NPM_INSTALL)

clean-wp:
	rm -rf out

clean-site:
	rm -rf _site _cache

nodrafts: 
	find posts -type l -exec rm -vf {} \+

$(SETUP):
	stack setup
	touch $@

$(NPM_INSTALL): package.json
	npm install
	touch $@

$(NPM_BUILD): $(NPM_INSTALL) $(wildcard css/*)
	npm run build
	touch $@

$(STACK_BUILD): package.yaml stack.yaml site.hs $(wildcard src/*.hs)
	stack build 
	touch $@

site-build: $(SETUP) $(NPM_INSTALL) $(STACK_BUILD) $(NPM_BUILD)
	stack exec -- site build
