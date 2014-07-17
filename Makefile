hython: *.hs
	cabal build
	ln -sf dist/build/hython/hython .

lint:
	cabal exec hlint .

.PHONY: test
test:
	python3 test.py

clean:
	cabal clean
