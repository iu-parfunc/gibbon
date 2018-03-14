#!/usr/bin/env bash

set -xe

echo "Running on machine: "`hostname -a || echo env says $HOSTNAME`
uname -a

echo "Git commit:"
(git log | head) || echo ok
echo "Git commit depth: "
(git log --pretty=oneline | wc -l) || echo ok

top=`pwd`

# This testing mode assumes that nix/docker integration is OFF by default:
export STACKARGS="--no-system-ghc"

if [ "$DOCKER" == "1" ]; then
    # Make bintree-bench image:
    cd $top/BintreeBench
    make docker

    # Make dependent image:
    cd $top/
    docker build -t tree-velocity .

elif [ "$USE_NIX" == "1" ]; then

    # Here by default we use a pinned version of the software ecosystem:
    nix-shell --pure --command  "./run_all_tests.sh $@"

else

    module add gcc
    ./run_all_tests.sh $@

fi
