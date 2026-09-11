CLANG_BIN=/usr/bin/
CC= $(CLANG_BIN)clang++
OPTIONS=-O3 -march=native #-fopenmp=libiomp5
FILES=implementations.cpp

GCC=g++

all : clang


clang:
	$(CC) $(OPTIONS) $(INCLUDE) $(FILES) -o main


gcc:
	g++ $(OPTIONS) $(FILES) -o main
