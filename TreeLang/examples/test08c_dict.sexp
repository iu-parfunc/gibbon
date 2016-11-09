#lang s-exp "../treelang.rkt"

(let ([d : (SymDict Int) (ann (empty-dict) (SymDict Int))])
  (let ([d2 : (SymDict Int) (insert d 1 (ann 2 Int))])
    (ann (lookup d2 1) Int)))
