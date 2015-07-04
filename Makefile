SOURCES = $(shell find src -type f -name '*.hs' -o -name '*.y')

hython: hython.cabal $(SOURCES)
	@stack build
	@stack exec hlint -- src --ignore="Eta reduce"
	@ln -sf $(shell stack path --dist-dir)/build/hython/hython .

.PHONY: test
test: hython
	@python3 test.py

clean:
	@stack clean
