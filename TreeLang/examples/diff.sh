#!/bin/bash

set -e

A=`tempfile`
B=`tempfile`

grep -v SELFTIMED $1 > $A
grep -v SELFTIMED $2 > $B

diff $A $B

