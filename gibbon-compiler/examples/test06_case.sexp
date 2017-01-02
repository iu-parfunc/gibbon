#lang gibbon

(data Foo (MkFoo Int))

(case (MkFoo 33)
  [(MkFoo n) n])
