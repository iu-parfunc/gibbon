#lang s-exp "../../gibbon/main.rkt"

(if (and True
         (or False False (or) True)
         (and))
    1
    0)
