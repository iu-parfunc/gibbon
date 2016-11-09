#lang s-exp "../treelang.rkt"

(let ([d : (SymDict Int) (empty-dict Int)])
  (let ([d2 : (SymDict Int) (insert Int d 1 2)])
    (let ([d3 : (SymDict Int) (insert Int d2 2 5)])
      (let ([d4 : (SymDict Int) (insert Int d2 2 10)])
        (+ (lookup Int d4 2) (lookup Int d3 2))))))
