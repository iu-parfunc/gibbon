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

# Set TREELANGDIR:
source set_env.sh

export PLTADDONDIR=`pwd`/.racket_sandbox/

# If we TRUST stack and racket, then leaving this out can speed up our incremental CI tests.
#echo "  Clean the working copy"
#echo "----------------------------------------"
# cd $top/; make clean


set +x; echo
echo "  Racket code (1/2)"
echo "----------------------------------------"
set -x

# First the core #lang implementation:
cd $top/; make racket


set +x; echo
echo "  Racket code (2/2)"
echo "----------------------------------------"
set -x

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


set +x; echo
echo "  Gibbon Compiler"
echo "----------------------------------------"
set -x
cd $top/gibbon-compiler

# Run compiler unit tests 
stack --allow-different-user --install-ghc test "$STACKARGS"

cd $top/gibbon-compiler/examples
make test $MKPARARGS
# Turning of -j for now [2016.11.06]



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


# [2017.01.11] Disabling for now.  Adds dependencies and we aren't using it:
# set +x; echo
# echo "  Handwritten Parser:"
# echo "----------------------------------------"
# set -x
# cd $top/deps/sexpr-1.3
# make || ./configure && make $MKPARARGS
# # TODO: Move to common:
# cd $top/ASTBenchmarks/common/c/
# make check

