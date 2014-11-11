all:
	cabal install --force-reinstalls && cabal run
spec:
	cabal install && cabal spec
