#!/bin/bash

set -xe

top=`pwd`

hostname
uname -a
which -a stack
stack --version

# ----------------------------------------
cd $top/BintreeBench
if [ "$DOCKER" == "1"]; then
    docker build . -t bintree-bench
else
    make
    make run_small
fi

# ----------------------------------------
cd $top/PackedTreesFormal
if [ "$DOCKER" == "1"]; then
    STACKARG="--docker"
    stack docker pull
else
    STACKARG=""
fi

stack --install-ghc test "$STACKARG"
