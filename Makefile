LINTABLE_SOURCES = $(shell find src -type f -name '*.hs' ! -name "Lexer.hs" ! -name "Parser.hs")

hython: hython.cabal src/Language/Python/* src/* src/Hython/*
	@cabal build
	-@./.cabal-sandbox/bin/hlint $(LINTABLE_SOURCES)
	@ln -sf dist/build/hython/hython .

.PHONY: test
test: hython
	@python3 test.py

clean:
	@cabal clean --verbose=0
