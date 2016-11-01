#!/bin/bash

# Allow failures so we can keep going:
set +e

for f in `find ../cleaned_racket -name "*.sexp" `; do
    echo "Processing file " $f
    ./check.exe $f 1 
    exit
done
