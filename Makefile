#


.PHONY: nodrafts rebuild clean clean-wp clean-site site-build site-rebuild prettier

SETUP=.done.setup

STACK_BUILD = .done.stack-build
PY_SETUP = .done.py-setup

PY_SVG_GEN = $(wildcard images/*/*.svg.py)
PY_SVGS = $(patsubst %.svg.py,%.svg,$(PY_SVG_GEN))

PY_VENV = ./.venv
PYTHON = $(PY_VENV)/bin/python

all: site-build

clean: clean-wp clean-flags clean-site clean-gen
clean-flags:
	rm -f $(wildcard .done.*)

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

$(STACK_BUILD): $(SETUP) package.yaml stack.yaml \
	 hakyll-site/main.hs \
	 slick-site/main.hs \
	 $(wildcard src/*.hs)
	stack build
	touch $@

$(PYTHON):
	virtualenv -p python2 $(PY_VENV)

$(PY_SETUP): $(PYTHON) requirements.txt
	$(PY_VENV)/bin/pip install -r requirements.txt
	touch $@

$(PY_SVGS): %.svg : %.svg.py $(PY_SETUP)
	tmp=$$(mktemp) && \
	$(PYTHON) $< > "$$tmp" && \
	mv -v "$$tmp" $@

site-build site-rebuild: site-%: $(STACK_BUILD) posts/*.md $(PY_SVGS)
	stack exec -- slick site-$*

watchexec-%:
	watchexec -- $(MAKE) $*

serve: $(STACK_BUILD)
	stack exec -- site server --host 0.0.0.0


prettier:
	yarn run prettier

