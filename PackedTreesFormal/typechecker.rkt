#lang s-exp "treelang.rkt"

;; use structs/data instead of sexp
(provide typecheck-expr
         Int_ Bool_ Lamt P S N B Begin Lam App)

(data Type
      [Int_]
      [Bool_]
      [Lamt (Listof Type) Type])

(data Param
      [P Expr Type])

(data Expr
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
    [(Empty) (error 'lookup-env "not in env: ~s~n" sym)]
    [(Extend-Env s expr e_)
     (if (eq? s sym)
         expr
         (lookup-env e_ sym))]))

;; empty?/car/cdr not in lang
(define (typecheck-begin [exprs : (Listof Expr)] [env : Env]) : Type
  (if (empty? (cdr exprs)) ;; need something like this?
      (typecheck (car exprs) env)
      (let ()
        (typecheck (car exprs) env)
        (typecheck-begin (cdr exprs) env))))

;; need to extend env
(define (get-param-types [params : (Listof Param)] [env : Env]) : (Listof Type)
  (if (empty? params)
      '()
      (case (car params)
        [(P s t)
         (cons t (get-param-types (cdr params)
                                  env))])))

(define (lam-extend-env [params : (Listof Param)] [env : Env]) : Env
  (if (empty? params)
      env
      (case (car params)
        [(P s t)
         (case s
           [(S sym)
            (extend-env env sym t)])])))

;; errors or returns #t
(define (helper [l1 : (Listof Type)] [l2 : (Listof Type)]) : Bool
  (if (and (empty? l1)
           (empty? l2))
      #t
      (type-equal? (car l1) (car l2)))) ;; don't use and since type-equal errors or returns #t

;; this function will error or return true. for now
(define (type-equal? [t1 : Type] [t2 : Type]) : Bool
  (case t1
    [(Int_)
     (case t2
       [(Int_) #t])] ;; will error in some fashion if not equal.
    [(Bool_)
     (case t2
       [(Bool_) #t])]
    [(Lamt pt bt)
     (case t2
       [(Lamt pt2 bt2)
        (let ()
          (helper pt pt2)
          (type-equal? bt bt2))])]))
        

;; car/cdr will fail if they are not of same length. don't do error checking
(define (params-args-equal? [ptypes : (Listof Type)] [args : (Listof Expr)] [env : Env]) : Bool
  (if (and (empty? ptypes)
           (empty? args))
      #t
      (type-equal? (car ptypes) (typecheck (car args) env))))

(define (typecheck [expr : Expr] [env : Env] ): Type
  (case expr
    [(S sym)
     (lookup-env env sym)]
    [(N n)
     (Int_)]
    [(B b)
     (Bool_)]
    [(Begin ls)
     (typecheck-begin ls env)]
    [(Lam params body)
     (Lamt (get-param-types params env)
           (typecheck body (lam-extend-env params env)))]
    [(App lam args)
     (case (typecheck lam env)
       [(Lamt ptypes btype)
        (let ()
          (params-args-equal? ptypes args env) ;; #t or error
          btype)])]))

;; (let ([v : t e] ...) e)    

(define (typecheck-expr [expr : Expr]) : Type
  (typecheck expr (Empty)))

