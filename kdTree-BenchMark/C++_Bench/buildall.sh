#!/bin/bash
echo "clear all binaries" rm -r ./bin
mkdir bin
echo "building unpacked..."
g++ -std=c++11 -O3 -Wall ./src/pointCorrelation_unpacked/main.cpp ./src/pointCorrelation_unpacked/build_tree.cpp ./src/pointCorrelation_unpacked/traversals_unpacked.cpp  -o ./bin/unpacked

echo "building bumpalloc..."
g++ -std=c++11 -O3 -Wall ./src/pointCorrelation_bumpalloc/main.cpp ./src/pointCorrelation_bumpalloc/build_tree.cpp ./src/pointCorrelation_bumpalloc/traversals_bumpalloc.cpp  -o ./bin/bumpalloc

echo "building packed..."
g++ -std=c++11 -O3 -Wall ./src/pointCorrelation_packed/main.cpp ./src/pointCorrelation_packed/build_tree.cpp ./src/pointCorrelation_packed/traversals_packed.cpp  -o ./bin/packed

echo "building packed enhanced..."
g++ -std=c++11 -O3 -Wall ./src/pointCorrelation_packed_enhanced/main.cpp ./src/pointCorrelation_packed_enhanced/build_tree.cpp ./src/pointCorrelation_packed_enhanced/traversals_packed.cpp  -o ./bin/packed_enhanced
