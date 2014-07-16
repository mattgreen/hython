hython: *.hs
	cabal build
	ln -sf dist/build/hython/hython .

lint:
	cabal exec hlint .

clean:
	cabal clean
