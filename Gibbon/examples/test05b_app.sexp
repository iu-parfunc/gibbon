#lang s-exp "../treelang.rkt"

(define (dbl [x : Int]) : Int
  (+ x x))

(dbl (dbl 3))
