#lang s-exp "../treelang.rkt"

(data Foo (MkFoo Int))

(let ((x : Foo (time (MkFoo 3))))
  99)
