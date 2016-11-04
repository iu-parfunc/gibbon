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

source set_env.sh

set +x; echo
echo "  Racket code"
echo "----------------------------------------"
set -x
cd $top/ASTBenchmarks/common/racket; make
cd $top/ASTBenchmarks/substitution/treelang; make

raco make -v $top/ASTBenchmarks/substitution/racket/subst.rkt \
             $top/ASTBenchmarks/rewrite.rkt

racket $top/typecheck-stlc/examples.rkt

# If we wanted to be really aggressive we could run all racket files
# in the Repo:
racket $top/ASTBenchmarks/tests/*.rkt


set +x; echo
echo "  Compiler"
echo "----------------------------------------"
set -x
cd $top/TreeLang

# Run compiler unit tests 
stack --install-ghc test "$STACKARGS"

cd $top/TreeLang/examples
make test



set +x; echo
echo "  Handwritten Parser:"
echo "----------------------------------------"
set -x
cd $top/deps/sexpr-1.3
make || ./configure && make
# TODO: Move to common:
cd $top/ASTBenchmarks/common/c/
make check


if [ "$NOBINTREE" != "1" ]; then 
  set +x; echo
  echo "  Bintree Microbench:"
  echo "----------------------------------------"
  set -x
  cd $top/BintreeBench
  if [ "$DOCKER" == "1" ]; then
      echo "Building under Docker, FULL benchmark set."
      make
      make run_small
  else
      echo "Not under Docker. Don't do a full Bintree build, it requires too many toolchains."
      make c ghc run_small_core
  fi
fi
