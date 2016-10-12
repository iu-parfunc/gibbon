#!/bin/bash

# Harvest expanded Racket from ALL racket files in the current directory.

# This simple version of the script doesn't follow dependencies or
# package boundaries...  It just runs rampant over any rkt files it
# can find.

set -xe

RACO=`which raco`
IN=$1
OUT=$1.out.sexp

raco help

set +xe

for f in `find -name "*.rkt"`; do
    echo "Processing $f"
    OUT=$f.out.sexp
    #    harvest.sh "$f"
    ${RACO} expand ${f} > ${OUT}
done

tar czvf all_expanded_files.tgz `find -name "*.out.sexp"`
