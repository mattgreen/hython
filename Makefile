hython: AST.hs Parser.hs Interpreter.hs
	cabal build

lint:
	cabal exec hlint .

clean:
	cabal clean
