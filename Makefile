all: install install-lib

install:
	cabal install --overwrite-policy=always

install-lib:
	cp std/*.ata ${HOME}/.atacamite/