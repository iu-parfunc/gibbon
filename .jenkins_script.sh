#!/bin/bash

set -xe

top=`pwd`

hostname
uname -a
which -a stack
which -a racket
which -a gcc
stack --version
racket --version
gcc --version

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
# Don't do a full build, it requires too many toolchains:    
#    make
#    make run_small
    make c ghc
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

