#!/usr/bin/env bash

resolver="19.19" # stack ls snapshots --lts remote | cat
pkgs=()
pack=( "${pkgs[@]/#/--package }")

cd src
if [ "$1" == "ghci" ]; then
    stack ghci Main.hs --resolver lts-$resolver ${pack[@]}
    exit 0
else
    stack ghc Main.hs --resolver lts-$resolver ${pack[@]} -- -Wall -o ../atacamite
    s=$?
    rm -f *.hi *.o
    echo "Done!"
    exit $s
fi