hython: AST.hs Parser.hs Interpreter.hs
	cabal build
	ln -s dist/build/hython/hython .

lint:
	cabal exec hlint .

clean:
	cabal clean
