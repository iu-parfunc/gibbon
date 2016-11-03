#lang s-exp "../treelang.rkt"

(let ([b : Bool (vector-ref (vector 1 True) 1)])
  (vector-ref (vector 1 True) 0))
