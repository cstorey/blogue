#


.PHONY: nodrafts rebuild clean clean-wp clean-site site-build site-rebuild prettier

SETUP=.done.setup

STACK_BUILD = .done.stack-build

all: $(STACK_BUILD)
	stack exec -- slick

clean: $(STACK_BUILD)
	stack exec -- slick clean

nodrafts:
	find posts -type l -exec rm -vf {} \+

$(SETUP):
	stack setup
	touch $@

$(STACK_BUILD): $(SETUP) package.yaml stack.yaml \
	 hakyll-site/main.hs \
	 slick-site/main.hs \
	 $(wildcard src/*.hs)
	stack build
	touch $@

site-build site-rebuild: site-%: $(STACK_BUILD) posts/*.md
	stack exec -- slick site-$*

watchexec-%:
	watchexec -- $(MAKE) $*

serve: $(STACK_BUILD)
	stack exec -- site server --host 0.0.0.0

prettier:
	yarn run prettier

