#lang typed/racket/base

(require "command_line_runner.rkt")

(define PASSNAME "treebench")

;; variant: treelang-racket

(launch-benchmarks "./treebench_treelang.rkt" PASSNAME "treelang-racket")

(launch-benchmarks "./treebench_packed_structs.exe" PASSNAME "treelang-c-packed")

(launch-benchmarks "./treebench.exe" PASSNAME "treelang-c-pointer")

;; add more here