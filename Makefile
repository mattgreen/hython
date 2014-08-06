hython: src/Lexer.hs src/Parser.hs src/Language.hs src/Interpreter.hs
	@cabal build
	-@./.cabal-sandbox/bin/hlint .
	@ln -sf dist/build/hython/hython .

src/Lexer.hs: src/Lexer.x
	./.cabal-sandbox/bin/alex -o src/Lexer.hs src/Lexer.x

src/Parser.hs: src/Parser.y
	./.cabal-sandbox/bin/happy -o src/Parser.hs src/Parser.y

.PHONY: test
test: hython
	@python3 test.py

clean:
	@cabal clean --verbose=0
