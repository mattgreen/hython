SOURCES = $(shell find src -type f -name '*.hs' -o -name '*.x' -o -name '*.y')

hython: hython.cabal $(SOURCES)
	@cabal build
	-@./.cabal-sandbox/bin/hlint src
	@ln -sf dist/build/hython/hython .

.PHONY: test
test: hython
	@python3 test.py

clean:
	@cabal clean --verbose=0
