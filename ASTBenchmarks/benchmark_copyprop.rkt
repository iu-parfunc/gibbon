#lang typed/racket/base

(require "common/racket/benchmark_runner.rkt")

(define PASSNAME "copy-prop")

(launch-benchmarks "racket ./copyprop/treelang/run_one_benchmark.rkt" PASSNAME "treelang-racket")
