#!/bin/bash

mkdir runlogs
mkdir results
echo "benchmarking unpakced version......"
NOW=$(date +"%Y-%m-%d-%H-%M-%S")

echo "running updatetree"
./bin/unpacked $1 $2 updatetree >> ./runlogs/"log_unpacked""$NOW"
echo "running intout"
./bin/unpacked $1 $2    intout >>      ./runlogs/"log_unpacked""$NOW"
echo "running treeout"
./bin/unpacked $1 $2 treeout >>     ./runlogs/"log_unpacked""$NOW"

grep '^res:\['  ./runlogs/"log_unpacked""$NOW" |sed -e 's/res:\[//g' -e 's/\]//g'  >>./results/"data_benchmark_all_$1_$2_""$NOW"".csv"

echo "benchmarking bumpalloc version......"
echo "running updatetree"
./bin/bumpalloc $1 $2 updatetree >> ./runlogs/"log_bumpalloc""$NOW"
echo "running intout"
./bin/bumpalloc $1 $2    intout >>      ./runlogs/"log_bumpalloc""$NOW"
echo "running treeout"
./bin/bumpalloc $1 $2 treeout >>     ./runlogs/"log_bumpalloc""$NOW"

grep '^res:\['  ./runlogs/"log_bumpalloc""$NOW" |sed -e 's/res:\[//g' -e 's/\]//g'  >>./results/"data_benchmark_all_$1_$2_""$NOW"".csv"

echo "benchmarking packed version......"
echo "running updatetree"
./bin/packed $1 $2 updatetree >> ./runlogs/"log_packed""$NOW"
echo "running intout"
./bin/packed $1 $2    intout >>      ./runlogs/"log_packed""$NOW"
echo "running treeout"
./bin/packed $1 $2 treeout >>     ./runlogs/"log_packed""$NOW"

grep '^res:\['  ./runlogs/"log_packed""$NOW" |sed -e 's/res:\[//g' -e 's/\]//g'  >>./results/"data_benchmark_all_$1_$2_""$NOW"".csv"
