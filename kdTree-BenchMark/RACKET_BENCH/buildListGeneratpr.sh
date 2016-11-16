#!/bin/bash


mkdir bin
rm ./bin/inputGenerator
echo "building input generator..."
g++ -std=c++11 -O3 -Wall ./input_generator/main.cpp ./input_generator/build_tree.cpp  -o ./bin/inputGenerator

