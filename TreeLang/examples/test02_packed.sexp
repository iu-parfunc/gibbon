#lang s-exp "../TreeLang/treelang.rkt"

(data Packed (MkPacked Int))

(define (eat [x : Packed]) : Int
  (case x
    [(MkPacked n) n]))

(eat (MkPacked 3))
