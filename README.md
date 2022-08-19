# Atacamite
A (soon to be) statically typed, stack-based programming language implemented in Haskell.

Take a look at  [examples](https://github.com/azur1s/atacamite/tree/main/examples) directory and try out some programs!

### Building
If you want to install from source, or to work on it, you should use `cabal` for building.
```
git clone https://github.com/azur1s/atacamite.git
cd atacamite
cabal build

cabal run
cabal run -- <atacamite's arguments>
```
If you want to install `atacamite` on your system and also install the standard library, you can just run
```
make
```
it will run `cabal install` and then copies the standard library for you. The binary will be installed to `$HOME/.cabal/bin/atacamite`