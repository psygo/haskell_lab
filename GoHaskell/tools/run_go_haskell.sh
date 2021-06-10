#!/bin/bash

cd src

ghc --make -dynamic GoHaskell.hs \
  -o ../compiled/GoHaskell \
  || exit 1

cd ..

./compiled/GoHaskell || exit 2

bash ./tools/cleanup.sh || exit 3

exit 0
