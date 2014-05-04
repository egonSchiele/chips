all:
	cabal install && cabal run
repo:
	new_bitbucket_repo chips
spec:
	cabal install && cabal spec
