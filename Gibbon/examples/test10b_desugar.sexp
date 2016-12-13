#lang s-exp "../gibbon.rkt"

(if (and True
         (or False False (or) True)
         (and))
    1
    0)
