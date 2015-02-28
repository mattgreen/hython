SOURCES = $(shell find src -type f -name '*.hs' -o -name '*.y')

hython: hython.cabal $(SOURCES)
	@cabal build
	-@./.cabal-sandbox/bin/hlint src --ignore="Eta reduce"
	@ln -sf dist/build/hython/hython .

install-deps:
	@cabal install --only-dependencies
	@cabal install happy hlint

.PHONY: test
test: hython
	@python3 test.py

clean:
	@cabal clean --verbose=0
