#lang typed/racket/base

;; Run make buildtree before this

(require "command_line_runner.rkt")

(define PASSNAME "buildtree")

(launch-benchmarks "./buildtree_treelang_c_packed.exe -benchmark " PASSNAME "treelang-c-packed")

(launch-benchmarks "racket buildtree_treelang.rkt" PASSNAME "treelang-racket")
