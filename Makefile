SOURCES = $(shell find src -type f -name '*.hs' -o -name '*.y')

hython: hython.cabal stack.yaml $(SOURCES)
	@stack build -j4
	@stack exec hlint -- src
	@ln -f $(shell stack path --dist-dir)/build/hython/hython .

.PHONY: test
test: hython
	@python3 test.py

clean:
	@stack clean
