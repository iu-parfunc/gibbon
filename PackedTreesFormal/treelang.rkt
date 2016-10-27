#lang typed/racket

(provide Int Sym Bool SymDict data empty-dict lookup insert case
         (all-from-out typed/racket)) ;; so i have lists for now

#| Grammar

prog := d ... f ... e

;; done
d := (data T [K t ...] ...)

;; done
f := (define (f [v : t] ...) : t e)

;; done
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

primapp := (binop e e)
        | (insert e e e)
        | (lookup e e)

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
                                  


