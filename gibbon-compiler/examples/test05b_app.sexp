#lang s-exp "../../gibbon/main.rkt"

(define (dbl [x : Int]) : Int
  (+ x x))

(dbl (dbl 3))
