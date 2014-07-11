hython: AST.hs Parser.hs Interpreter.hs
	ghc -outputdir ./obj Interpreter.hs -o hython

clean:
	rm -f ./hython
