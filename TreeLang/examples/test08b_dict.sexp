#lang s-exp "../treelang.rkt"

(let ([d : (SymDict Int) (ann (empty-dict) (SymDict Int))])
  (let ([d2 : (SymDict Int) (insert d 10 (ann 200 Int))])
    44))
