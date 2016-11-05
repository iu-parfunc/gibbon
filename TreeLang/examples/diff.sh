#!/bin/bash

set -e

function checkfile() {
    if ! [ -e "$1" ]; then
        echo "File does not exist, cannot diff!: " $1
        exit 1
    fi
}

checkfile $1
checkfile $2

A=`mktemp`
B=`mktemp`

grep -v SELFTIMED $1 > $A
grep -v SELFTIMED $2 > $B

diff $A $B
code=$?

rm $A $B

exit $code
