hython: src/Lexer.hs src/Parser.hs src/Language.hs src/Interpreter.hs hython.cabal
	@cabal build
	-@./.cabal-sandbox/bin/hlint src/Language.hs src/Interpreter.hs
	@ln -sf dist/build/hython/hython .

src/Lexer.hs: src/Lexer.x
	./.cabal-sandbox/bin/alex -g -o src/Lexer.hs src/Lexer.x

src/Parser.hs: src/Parser.y
	./.cabal-sandbox/bin/happy -a -g -c -o src/Parser.hs src/Parser.y

.PHONY: test
test: hython
	@python3 test.py

clean:
	@cabal clean --verbose=0
