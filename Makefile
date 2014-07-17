hython: *.hs
	@cabal build
	@./.cabal-sandbox/bin/hlint -q .
	@ln -sf dist/build/hython/hython .

.PHONY: test
test:
	@python3 test.py

clean:
	@cabal clean --verbose=0
