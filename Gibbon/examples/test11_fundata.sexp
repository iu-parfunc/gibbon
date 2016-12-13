#lang s-exp "../gibbon.rkt"

(data Foo (MkFoo Int))

;; Should generate an end-of-input witness as a return value:
(define (foo [x : Foo]) : Int
  (case x
    [(MkFoo n) n]))

500
