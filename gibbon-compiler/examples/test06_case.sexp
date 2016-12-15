#lang s-exp "../../gibbon/main.rkt"

(data Foo (MkFoo Int))

(case (MkFoo 33)
  [(MkFoo n) n])
