#!/bin/bash

# Allow failures so we can keep going:
set +e

for f in `find ../expanded_racket -name "*.sexp" `; do
    ./subst_c.exe call-with-values $f 10 
done
