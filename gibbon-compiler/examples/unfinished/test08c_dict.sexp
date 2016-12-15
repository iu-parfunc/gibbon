#lang s-exp "../../gibbon/main.rkt"

(let ([d : (SymDict Int) (empty-dict Int)])
  (let ([d2 : (SymDict Int) (insert Int d 1 2)])
    (lookup Int d2 1)))
