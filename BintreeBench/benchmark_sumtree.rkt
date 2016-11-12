#lang typed/racket/base

;; WARNING: Code duplication with benchmark_buildtree.rkt

(require "command_line_runner.rkt" racket/system)

(system "make sumtree")

(define PASSNAME "sumtree")

(launch-benchmarks "./sumtree_treelang_c_packed.exe -benchmark " PASSNAME "treelang-c-packed")

(launch-benchmarks "racket sumtree_treelang.sexp " PASSNAME "treelang-racket")

(launch-benchmarks "racket treebench.rkt sum " PASSNAME "handwritten-racket")

(launch-benchmarks "java treebench sum " PASSNAME "handwritten-java")

(launch-benchmarks "scheme --script treebench.ss sum " PASSNAME "handwritten-chez")

(launch-benchmarks "./treebench_rust.exe sum " PASSNAME "handwritten-rust")
