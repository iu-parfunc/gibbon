#!/bin/bash

set -xe

echo "Running on machine: "`hostname -a`
uname -a

top=`pwd`

if [ "$DOCKER" == "1" ]; then
    # Make bintree-bench image:
    cd $top/BintreeBench
    make docker

    # Make dependent image:
    cd $top/
    docker build -t tree-velocity .
else

    ./run_all_tests.sh

fi
