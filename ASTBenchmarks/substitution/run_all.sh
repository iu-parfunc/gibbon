#!/bin/bash

set -e

for f in `find ../expanded_racket -name "*.sexp" `; do
    echo "Benchmarking $f"
    ./subst.rkt call-with-values $f 1
done
