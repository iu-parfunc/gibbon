#lang s-exp "../gibbon.rkt"

(data Foo (MkFoo Int))

(case (MkFoo 33)
  [(MkFoo n) n])
