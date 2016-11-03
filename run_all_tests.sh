#!/bin/bash

# Responds to environment variables:
#   * DOCKER
#   * STACKARGS 

set -xe

cd `dirname $0`
top=`pwd`

hostname
uname -a
which -a stack
which -a racket
which -a gcc
stack --version
racket --version
gcc --version


set +x; echo
echo "  Racket code"
echo "----------------------------------------"
set -x
cd $top/ASTBenchmarks/common/racket; make
cd $top/ASTBenchmarks/substitution/treelang; make

raco make -v $top/ASTBenchmarks/substitution/racket/subst.rkt \
             $top/ASTBenchmarks/rewrite.rkt

racket $top/typecheck-stlc/examples.rkt


set +x; echo
echo "  Compiler"
echo "----------------------------------------"
set -x
cd $top/TreeLang

# Run compiler unit tests 
stack --install-ghc test "$STACKARGS"

DEBUG=2 stack exec -- packed-trees examples/test00_add.sexp


set +x; echo
echo "  Handwritten Parser:"
echo "----------------------------------------"
set -x
cd $top/deps/sexpr-1.3
make
# TODO: Move to common:
cd $top/ASTBenchmarks/substitution/
make check


set +x; echo
echo "  Bintree Microbench:"xs
echo "----------------------------------------"
set -x
cd $top/BintreeBench
if [ "$DOCKER" == "1" ]; then
    echo "Building under Docker, FULL benchmark set."
    make
    make run_small
else
    # Don't do a full build, it requires too many toolchains:    
    make c ghc run_small_core
fi
