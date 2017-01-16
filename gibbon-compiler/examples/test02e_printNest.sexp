#lang gibbon

(data Foo (MkFoo Int Int))
(data Bar (MkBar Foo Foo))
(data Quux (MkQuux Bar))

(MkQuux (MkBar (MkFoo 1 2) (MkFoo 3 4)))
