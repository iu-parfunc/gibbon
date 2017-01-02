#lang gibbon

;; This is an example of the binary packing facility.
(provide (all-defined-out))

(data Foo (MkFoo Int))

(data Bar (MkBar Foo))

