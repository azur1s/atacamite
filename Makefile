all: bin lib

bin:
	cabal install --overwrite-policy=always

lib:
	cp std/*.ata ${HOME}/.atacamite/