hython: src/Small.hs src/Lexer.hs src/Parser.hs src/Language.hs src/Interpreter.hs
	@cabal build
	#-@./.cabal-sandbox/bin/hlint .
	@ln -sf dist/build/hython/hython .

src/Lexer.hs: src/Lexer.x
	./.cabal-sandbox/bin/alex -g -o src/Lexer.hs src/Lexer.x

src/Parser.hs: src/Parser.y
	./.cabal-sandbox/bin/happy -a -g -c -iinfo.txt -o src/Parser.hs src/Parser.y

src/Small.hs: src/Small.y
	./.cabal-sandbox/bin/happy -a -g -c -ismall.txt -o src/Small.hs src/Small.y

.PHONY: test
test: hython
	@python3 test.py

clean:
	@cabal clean --verbose=0
