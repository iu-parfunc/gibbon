#lang s-exp "../treelang.rkt"

(let ([d (empty-dict)])
  (let ([d2 (insert d 1 2)])
    (lookup d2 1)))
