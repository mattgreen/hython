hython: AST.hs Parser.hs Interpreter.hs
	ghc -Wall -outputdir ./obj Interpreter.hs -o hython

clean:
	rm -f ./obj/*
	rm -f ./hython
