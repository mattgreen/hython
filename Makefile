hython: src/Lexer.hs src/Parser.hs src/Language.hs src/Interpreter.hs
	@cabal build
	-@./.cabal-sandbox/bin/hlint .
	@ln -sf dist/build/hython/hython .

.PHONY: test
test: hython
	@python3 test.py

clean:
	@cabal clean --verbose=0
