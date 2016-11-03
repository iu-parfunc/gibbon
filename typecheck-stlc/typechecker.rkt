#lang s-exp "../TreeLang/treelang.rkt"

;; FIXME: need to wean this off of car/cdr onto for/list fold/list
(require (only-in racket/base car cdr))

;; use structs/data instead of sexp
(provide typecheck-expr
         Int_ Bool_ Lamt P S N B Begin Lam App)

(data Type
      [Int_]
      [Bool_]
      [NullT]
      [Lamt (Listof Type) Type])

(data Param
      [P Expr Type])

(data Expr
      [Null]
      [S Sym]
      [N Int]
      [B Bool] ;; leaving out null for now
      [Begin (Listof Expr)]
      [Lam (Listof Param) Expr]
      [App Expr (Listof Expr)])

;; environment
(data Env
      [Empty]
      [Extend-Env Sym Type Env])

(define (extend-env [e : Env] [sym : Sym] [type : Type]) : Env
  (Extend-Env sym type e))

;; need an equality function
(define (lookup-env [e : Env] [sym : Sym]) : Type
  (case e
    [(Empty) (error "not found in env")]
    [(Extend-Env s expr e_)
     (if (eq? s sym)
         expr
         (lookup-env e_ sym))]))

(define (typecheck-begin [exprs : (Listof Expr)] [env : Env]) : Type
  (if (empty? (cdr exprs)) 
      (typecheck (car exprs) env)
      (let ()
        (typecheck (car exprs) env)
        (typecheck-begin (cdr exprs) env))))

(define (lam-extend-env [params : (Listof Param)] [env : Env]) : Env
  (if (empty? params)
      env
      (case (car params)
        [(P s t)
         (case s
           [(S sym)
            (extend-env env sym t)])])))

;; car/cdr could error
(define (type-equal-list? [l1 : (Listof Type)] [l2 : (Listof Type)]) : Bool
  (if (and (empty? l1)
           (empty? l2))
      #t
      (and
       (type-equal? (car l1) (car l2))
       (type-equal-list? (cdr l1) (cdr l2)))))

(define (type-equal? [t1 : Type] [t2 : Type]) : Bool
  (case t1
    [(NullT)
     (case t2
       [(NullT) #t]
       [(Int_) #f]
       [(Bool_) #f]
       [(Lamt pt bt) #f])]
    [(Int_)
     (case t2
       [(Int_) #t]
       [(Bool_) #f]
       [(NullT) #f]
       [(Lamt pt bt) #f])]
    [(Bool_)
     (case t2
       [(Bool_) #t]
       [(Int_) #f]
       [(NullT) #f]
       [(Lamt pt bt) #f])]
    [(Lamt pt bt)
     (case t2
       [(Lamt pt2 bt2)
        (and
         (type-equal-list? pt pt2)
         (type-equal? bt bt2))]
       [(Int_) #f]
       [(Bool_) #f]
       [(NullT) #f])]))
        

;; car/cdr will fail if they are not of same length. don't do error checking
(define (params-args-equal? [ptypes : (Listof Type)] [args : (Listof Expr)] [env : Env]) : Bool
  (if (and (empty? ptypes)
           (empty? args))
      #t
      (and
       (type-equal? (car ptypes) (typecheck (car args) env))
       (params-args-equal? (cdr ptypes) (cdr args) env))))

(define (typecheck [expr : Expr] [env : Env] ): Type
  (case expr
    [(Null)
     (NullT)]
    [(S sym)
     (lookup-env env sym)]
    [(N n)
     (Int_)]
    [(B b)
     (Bool_)]
    [(Begin ls)
     (typecheck-begin ls env)]
    [(Lam params body)
     (Lamt (for/list ([p : Param params])
             (case p
               [(P s t)
                t]))
           (typecheck body (lam-extend-env params env)))]
    [(App lam args)
     (case (typecheck lam env)
       [(Lamt ptypes btype)
        (if (params-args-equal? ptypes args env)
            btype
            (error "no type"))])]))

;; (let ([v : t e] ...) e)    

(define (typecheck-expr [expr : Expr]) : Type
  (typecheck expr (Empty)))

