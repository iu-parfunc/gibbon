#!/bin/bash

set -xe

top=`pwd`

hostname
uname -a
which -a stack
stack --version

# Racket code
# ----------------------------------------

cd $top/ASTBenchmarks/common/racket; make
cd $top/ASTBenchmarks/substitution/treelang; make

raco make -v $top/ASTBenchmarks/substitution/racket/subst.rkt \
             $top/ASTBenchmarks/rewrite.rkt

racket $top/typecheck-stlc/examples.rkt


# Bintree microbenchmark
# ----------------------------------------
cd $top/BintreeBench
if [ "$DOCKER" == "1" ]; then
    echo "Building under Docker."
    docker build . -t bintree-bench
else
    make
    make run_small
fi

# Compiler
# ----------------------------------------
cd $top/TreeLang
if [ "$DOCKER" == "1" ]; then
    STACKARG="--docker"
    stack docker pull
else
    STACKARG=""
fi

# Run compiler unit tests 
stack --install-ghc test "$STACKARG"

