#lang racket

(require "../../BintreeBench/treebench_gibbon.rkt"
         rackunit)

(define tr10 (build-tree 10))

(define bs (pack-Tree tr10))

;; Check against a known size from our C program:
(check-equal? (bytes-length bs) 10239)

(display "bintree packing test passed.\n")
