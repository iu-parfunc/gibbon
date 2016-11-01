#lang typed/racket

(provide Int Sym Bool SymDict data empty-dict lookup insert case
         define let provide require if : for/list

         list and empty? error 
         eq? car cdr cons Listof         

         time + * -

         #%app #%module-begin #%datum quote
         only-in
         #;(all-from-out typed/racket))

;; add for/list  w/types

#| Grammar

prog := #lang s-exp "/path/to/treelang.rkt"
        d ... f ... e

;; Data definitions:
d := (data T [K fTy ...] ...)

;; Function definitions
f := (define (f [v : t] ...) : t e)

;; Field types
fTy := t | (Listof t)

;; Types
t := Int | Sym | Bool
   | (Vector t ...) 
   | (SymDict t) ;; maps symbols to t?
   | T

e := var | lit
   | (f e ...)                      
   | (vector e ...)                 
   | (vector-ref e int)             
   | (K e ...)                      
   | (case e [(K v ...) e] ...)
   | (let ([v : t e] ...) e)        :: CHANGED THIS, note the :
   | (if e e e)                     
   | primapp
   | (for/list ([v : t e])  e)      ;; not enforced that only loops over single list
   | (error string)
   | (time e)                       ;; time a benchmark

primapp := (binop e e)
        | (insert e e e)
        | (lookup e e)
        | (empty-dict)

binop := + | - | * 

lit := int | #t | #f

;; Example "hello world" program in this grammar:

(data Tree
       [Leaf Int]
       [Node Tree Tree])
'
(define (add1 [x : Tree]) : Tree
  (case x
    [(Leaf n)   (Leaf (+ n 1))]
    [(Node x y) (Node (add1 x) (add1 y))]))

|#

;;(case e [(K v ...) e] ...)
(define-syntax (case stx)
  (syntax-case stx ()
    [(case v [(S fs ...) rhs] ...)
     #'(match v
         [(S fs ...) rhs] ...)]))

;;(insert e e e)
(define-syntax-rule (insert ht key v)
  (hash-set ht key v))

(define-syntax-rule (lookup ht key)
  (hash-ref ht key))

(define-syntax-rule (empty-dict)
  (hash))

(define-type Int Integer)
(define-type Sym Symbol)
(define-type Bool Boolean)
(define-type (SymDict t) (HashTable Symbol t))

(define-syntax (data stx)
  (syntax-case stx ()
    [(_ type1 [ts f ...] ...)
     (with-syntax ([((f-ids ...) ...)
                    (map generate-temporaries (syntax->list #'((f ...) ...)))])
       #'(begin
           (define-type type1 (U ts ...))
           (struct ts ([f-ids : f] ...) #:transparent) ...))]))

#|
(data Tree
       [Leaf Int]
       [Node Tree Tree])

(define (add1 [x : Tree]) : Tree
  (case x
    [(Leaf n)   (Leaf (+ n 1))]
    [(Node x y) (Node (add1 x) (add1 y))]))
|#
                                  


