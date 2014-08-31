hython: src/Lexer.x src/Parser.y src/Language.hs src/Interpreter.hs src/Builtins.hs hython.cabal
	@cabal build
	-@./.cabal-sandbox/bin/hlint src/Language.hs src/Interpreter.hs src/Builtins.hs
	@ln -sf dist/build/hython/hython .

.PHONY: test
test: hython
	@python3 test.py

clean:
	@cabal clean --verbose=0
