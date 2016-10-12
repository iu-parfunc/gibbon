#!/bin/bash

# Harvest expanded Racket core syntax from an input file.

RACO=`which raco`
IN=$1
OUT=$1.out.sexp

${RACO} expand ${IN} > ${OUT}
