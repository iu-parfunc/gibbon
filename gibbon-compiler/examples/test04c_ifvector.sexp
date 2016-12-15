#lang s-exp "../../gibbon/main.rkt"

(vector-ref (if True
                (vector 10 20)
                (vector 30 40))
            0)
