#lang typed/racket/base

(require "command_line_runner.rkt")

(define PASSNAME "treebench")

;; variant: treelang-racket

;;(launch-benchmarks "racket treebench_treelang.rkt" PASSNAME "treelang-racket")

(launch-benchmarks "./treebench_c_bumpalloc.exe" PASSNAME "handwritten-c-pointer-bumpalloc")

;;(launch-benchmarks "./treebench_c_bumpalloc_unaligned.exe" PASSNAME ...)

(launch-benchmarks "./treebench_c.exe" PASSNAME "handwritten-c-pointer")

(launch-benchmarks "./treebench_c_packed.exe" PASSNAME "handwritten-c-packed")

;;(launch-benchmarks "./treebench_c_packed_loop.exe" PASSNAME ...)

;;(launch-benchmarks "./treebench_c_packed_parallel3.exe" PASSNAME ...)

;;(launch-benchmarks "./treebench_c_packed_structs.exe" PASSNAME ...)

;;(launch-benchmarks "./treebench_c_parallel.exe" PASSNAME ...)

;; add more here

;; NEW

(launch-benchmarks "./treebench_mlton.exe " PASSNAME "handwritten-mlton")

(launch-bencharks "racket treebench.rkt add1 " PASSNAME "handwritten-racket")

(launch-benchmarks "java treebench add1 " PASSNAME "handwritten-java")
