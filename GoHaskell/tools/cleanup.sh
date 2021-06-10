#!/bin/bash

find . -type f -name '*.hi'            -delete || exit 1
find . -type f -name '*.o'             -delete || exit 2
find . -type d -name '*dist-newstyle*' -delete || exit 3

exit 0
