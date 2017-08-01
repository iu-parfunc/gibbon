# tree-velocity

[![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=tree-velocity)](http://tester-lin.soic.indiana.edu:8080/job/tree-velocity/)

A collection of sub-projects related to benchmarking tree traversals and 
performing tree traversals on packed representations.  Here is a guide to 
the subdirectories:

 * [gibbon-compiler](gibbon-compiler) - the prototype compiler for the Gibbon language of packed tree traversals.

 * [ASTBenchmarks](ASTBenchmarks) - benchmark of treewalks (compiler passes) on ASTs written with Gibbon.
   Also includes scripts to fetch input datasets.

 * `BintreeBench` - a submodule containing the tiniest binary tree microbenchmark, implemented several different languages and compilers.
   
 * [core-harvest](core-harvest) - tools to harvest realistic, large ASTs (mainly Racket) from the wild.
 
 * [DEVLOG.md](DEVLOG.md) - detailed documentation for those hacking on this repository.
