#!/bin/bash

# Right now we're having a problem with treewalk/--packed not working on all ASTs.
# This runs each of them and makes a list of the successes and failures.

TOP=`pwd`
cd ..
source set_env.sh

# tc --bumpalloc --bench-fun treewalk ./treewalk/gibbon/treewalk_gibbon.rkt -o treewalk_pointer_bumpalloc.exe

cd $TOP/treewalk/gibbon/
tc --packed --bench-fun treewalk treewalk_gibbon.rkt -o ../../treewalk_packed.exe

cd $TOP/binary_racket/

rm -f ../packed_succeeded.txt ../packed_failed.txt

# for file in cat ../debug/packed_succeeded.txt; do

for file in `find ./ -name "*.gpkd"`; do
    echo
    echo 
    echo "Trying --packed mode on: $file"
    ../treewalk_packed.exe --bench-input $file
    if [ $? == 0 ]; then
        echo "SUCCESS"
        echo $file >> ../packed_succeeded.txt
    else
        echo "FAILED."
        echo $file >> ../packed_failed.txt
    fi
done
