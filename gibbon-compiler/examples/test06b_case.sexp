#lang s-exp "../gibbon.rkt"

(data Bar (MkBar Int Int))

(case (MkBar 33 44)
  [(MkBar x y) y])

