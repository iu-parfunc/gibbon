#!/bin/bash

# Allow failures so we can keep going:
set +e

for f in `find ../expanded_racket -name "*.sexp" `; do
    ./subst.rkt call-with-values $f 1 
done
