#lang s-exp "../treelang.rkt"

(let ([d : (SymDict Int) (ann (empty-dict) (SymDict Int))])
  (let ([d2 : (SymDict Int) (insert d 1 (ann 2 Int))])
    (let ([d3 : (SymDict Int) (insert d2 2 (ann 5 Int))])
      (let ([d4 : (SymDict Int) (insert d2 2 (ann 10 Int))])
        (+ (ann (lookup d4 2) Int)
           (ann (lookup d3 2) Int))))))
