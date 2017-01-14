#!/bin/bash

ROOT=../../
BENCH_BIN=ASTBenchmarks/leaves
BINTREEFILE="bintree_leaf_"
EXE=".exe"
SEXP=".sexp"
BUILD="_build"
ADD1="_add1"
PACKED="_packed"
POINTER="_pointer"
BUMP="_bump"
SRCS=$BENCH_BIN"/srcs/"
BINS=$BENCH_BIN"/bins/"

# Generates gibbon progs which builds variable leaf sized bintrees
gen_files() 
{
  rm -rf $SRCS
  mkdir $SRCS
  for sz in 1 16 32 64 128
  do
    racket $BENCH_BIN/leaves.rkt -l $sz -o $SRCS$BINTREEFILE$sz$BUILD$SEXP -b build
    racket $BENCH_BIN/leaves.rkt -l $sz -o $SRCS$BINTREEFILE$sz$ADD1$SEXP -b add1
  done
}

compile() 
{
  rm -rf $BINS
  mkdir $BINS
  for sz in 1 16 32 64 128
  do
    for bench in $ADD1 $BUILD 
    do 
      tc --packed -o $BINS$BINTREEFILE$sz$bench$PACKED$EXE $SRCS$BINTREEFILE$sz$bench$SEXP 
      tc --pointer -o $BINS$BINTREEFILE$sz$bench$POINTER$EXE $SRCS$BINTREEFILE$sz$bench$SEXP
      tc --pointer --bumpalloc -o $BINS$BINTREEFILE$sz$bench$BUMP$EXE $SRCS$BINTREEFILE$sz$bench$SEXP
    done 
  done 
}

run() 
{
  echo -e "\nRunning each benchmark with 1000 iterations..\n\n"
  echo -e "Bench,Variant,LeafSize,PayloadBytes,SpineBytes,BatchTime" > $BENCH_BIN/leaves_results.csv

  N=`echo -e "2^15"|bc`
  for sz in 1 16 32 64 128
  do
    for bench in $ADD1 $BUILD 
    do 
      for variant in $BUMP $PACKED $POINTER
      do
        echo -e "-----------------------------------------------------------"
        echo -e "Running - Bench : $bench Leaf Size : $sz Variant : $variant"  
        echo -e "-----------------------------------------------------------"
        ./$BINS$BINTREEFILE$sz$bench$variant$EXE 0 1000  > temp.txt
        batchtime=`awk -F':' '/BATCHTIME*/ { print $2 }' temp.txt`
        echo -e "BATCHTIME: $batchtime"
        rm temp.txt
        if [ $variant = "_packed" ]; then
          spineBytes=`echo -e "8*(2*$N-1)" |bc`
          payloadBytes=`echo -e "8*$N*$sz" |bc`
          echo -e "${bench#*_},${variant#*_},$sz,$payloadBytes,$spineBytes,$batchtime" >> $BENCH_BIN/leaves_results.csv
        else
          spineBytes=`echo -e "8*(2*$N-1)+8*(2*$N-2)" |bc`
          payloadBytes=`echo -e "8*$N*$sz" |bc`
          echo -e "${bench#*_},${variant#*_},$sz,$payloadBytes,$spineBytes,$batchtime" >> $BENCH_BIN/leaves_results.csv
        fi
      done
    done
  done
}

(cd $ROOT;\
  source set_env.sh;\
  gen_files;\
  compile;\
  run)
