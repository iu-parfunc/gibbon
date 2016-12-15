#lang s-exp "../../gibbon/main.rkt"

(data Foo (MkFoo Int))

(let ((x : Foo (time (MkFoo 3))))
  99)
