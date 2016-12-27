#!/bin/bash

# Responds to environment variables:
#   * DOCKER
#   * STACKARGS 
#   * PAR

if [ "$1" == "-j" ]; then
    PAR=1
fi
if [ "$PAR" == "1" ]; then
    MKPARARGS="-j"
else
    MKPARARGS=""
fi

echo "Running full test suite, Parallelism flags = '$MKPARARGS' \n"
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
echo "  Compiler"
echo "----------------------------------------"
set -x
cd $top/gibbon-compiler

# Run compiler unit tests 
stack --allow-different-user --install-ghc test "$STACKARGS"

cd $top/gibbon-compiler/examples
make test $MKPARARGS
# Turning of -j for now [2016.11.06]



set +x; echo
echo "  Handwritten Parser:"
echo "----------------------------------------"
set -x
cd $top/deps/sexpr-1.3
make || ./configure && make $MKPARARGS
# TODO: Move to common:
cd $top/ASTBenchmarks/common/c/
make check



set +x; echo
echo "  Racket code"
echo "----------------------------------------"
set -x

# First the core #lang implementation:
cd $top/; make racket

# Then misc other code/benchmarks:
cd $top/ASTBenchmarks/common/racket; make
cd $top/ASTBenchmarks/substitution/treelang; make

raco make -v $top/ASTBenchmarks/substitution/racket/subst.rkt \
             $top/ASTBenchmarks/rewrite.rkt

racket $top/typecheck-stlc/examples.rkt

# If we wanted to be really aggressive we could run all racket files
# in the Repo:
racket $top/ASTBenchmarks/tests/*.rkt


# [2016.11.08] {Having problems with this -RRN}
# raco make -v $top/kdTree-BenchMark/racket/*.rkt
racket $top/kdTree-BenchMark/racket/traversal.rkt


if [ "$NOBINTREE" != "1" ]; then 
  set +x; echo
  echo "  Bintree Microbench:"
  echo "----------------------------------------"
  set -x
  cd $top/BintreeBench
  if [ "$DOCKER" == "1" ]; then
      echo "Building under Docker, FULL benchmark set."
      make $MKPARARGS
      make run_small
  else
      echo "Not under Docker. Don't do a full Bintree build, it requires too many toolchains."
      make c ghc -j
      make run_small_core
  fi
fi
