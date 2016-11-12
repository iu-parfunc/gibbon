#!/bin/bash

mkdir runlogs
mkdir results

echo "benchmarking packed enhanced version......"
echo "running updatetree"
./bin/packed_enhanced $1 $2 updatetree >> ./runlogs/"log_packed_enhanced""$NOW"
echo "running intout"
./bin/packed_enhanced $1 $2    intout >>      ./runlogs/"log_packed_enhanced""$NOW"
echo "running treeout"
./bin/packed_enhanced $1 $2 treeout >>     ./runlogs/"log_packed""$NOW"

grep '^res:\['  ./runlogs/"log_packed_enhanced""$NOW" |sed -e 's/res:\[//g' -e 's/\]//g'  >>./results/"data_benchmark_packed_enhanced_$1_$2_""$NOW"".csv"
