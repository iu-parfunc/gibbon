#lang s-exp "../gibbon.rkt"

(define (dbl [x : Int]) : Int
  (+ x x))

(dbl (dbl 3))
