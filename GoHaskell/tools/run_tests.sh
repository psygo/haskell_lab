#!/bin/bash

cd src/

ghc --make -dynamic Test/BoardTests.hs \
  -o ../compiled/BoardTests \
  || exit 1

cd ..

./compiled/BoardTests || exit 2

bash ./tools/cleanup.sh || exit 3

exit 0
