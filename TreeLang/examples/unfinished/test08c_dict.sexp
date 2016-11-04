#lang s-exp "../treelang.rkt"

(let ([d : (SymDict Int) (empty-dict)])
  (let ([d2 : (SymDict Int) (insert d 1 2)])
    (lookup d2 1)))
