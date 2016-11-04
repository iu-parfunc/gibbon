#!/bin/bash

set -e

A=`mktemp`
B=`mktemp`

grep -v SELFTIMED $1 > $A
grep -v SELFTIMED $2 > $B

diff $A $B

