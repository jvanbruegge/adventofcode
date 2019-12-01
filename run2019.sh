#!/bin/bash

if [ -z $1 ]; then
    echo "Usage: ./run2019.sh D<day (zero padded)>"
    exit 1
fi

cd 2019
stack runhaskell -- $1.hs
