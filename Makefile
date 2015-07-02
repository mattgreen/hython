SOURCES = $(shell find src -type f -name '*.hs' -o -name '*.y')
DIST_DIR = $(shell stack path | grep dist-dir | sed "s/dist-dir: //")

hython: hython.cabal $(SOURCES)
	@stack build
	@stack exec hlint -- src --ignore="Eta reduce"
	@ln -sf $(DIST_DIR)/build/hython/hython .

.PHONY: test
test: hython
	@python3 test.py

clean:
	@stack clean
