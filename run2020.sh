#!/bin/bash

if [ -z $1 ]; then
    echo "Usage: ./run2020.sh D<day (zero padded)>"
    exit 1
fi

cd 2020
stack runhaskell -- $1.hs
