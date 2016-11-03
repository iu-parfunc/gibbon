#lang s-exp "../treelang.rkt"

(let ([x : Bool (eq? True False)])
  (let ([y : Bool (= 3 4)])
    ;; Only printing/returning Ints right now, not bools:
    99))
