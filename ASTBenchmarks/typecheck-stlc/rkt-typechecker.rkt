#lang gibbon

;; use structs/data instead of sexp
(provide typecheck-expr test-typecheck Expr
         Int_ Bool_ Lamt NullT P S N B Begin Lam App Null
	 CONSEXPR NULLEXPR CONSPARAM NULLPARAM CONSTYPE NULLTYPE)

(data ListExpr
      [CONSEXPR Expr ListExpr]
      [NULLEXPR])

(data ListParam
      [CONSPARAM Param ListParam]
      [NULLPARAM])

(data ListType
      [CONSTYPE Type ListType]
      [NULLTYPE])      

(data Type
      [Int_]
      [Bool_]
      [NullT]
      [Lamt ListType Type]
      [Fail])

(data Param
      [P Expr Type])

(data Expr
      [Null]
      [S Sym]
      [N Int]
      [B Bool] ;; leaving out null for now
      [Begin ListExpr]
      [Lam ListParam Expr]
      [App Expr ListExpr])

(define (extend-env [a : Arena] [e : (SymDict Type)] [sym : Sym] [type : Type]) : (SymDict Type)
  (insert a e sym (ann type Type)))

(define (lookup-env [a : Arena] [e : (SymDict Type)] [sym : Sym]) : Type
  (ann (lookup e sym) Type))

(define (typecheck-begin [exprs : ListExpr] [a : Arena] [env : (SymDict Type)]) : Type
  (case exprs
    [(CONSEXPR e rest)
     (case rest
      [(CONSEXPR e2 rest2)
       (let ([t : Type (typecheck e a env)])
	 (case t
	   [(NullT) (typecheck-begin rest a env)]
	   [(Int_) (typecheck-begin rest a env)]
	   [(Bool_) (typecheck-begin rest a env)]
	   [(Lamt pt bt) (typecheck-begin rest a env)]
	   [(Fail) (Fail)]))]
      [(NULLEXPR)
       (typecheck e a env)])]
    [(NULLEXPR)
     (Fail)]))

(define (lam-extend-env [params : ListParam] [a : Arena] [env : (SymDict Type)]) : (SymDict Type)
  (case params
    [(CONSPARAM param rest)
     (let ([nenv : (SymDict Type) (case param
       	  	            	    [(P e t)
        		  	     (case e
	 		 	       [(S sym)
      		          	        (extend-env a env sym t)])])])
       (lam-extend-env rest a nenv))]
    [(NULLPARAM)
     env]))
     
(define (type-equal-list? [l1 : ListType] [l2 : ListType]) : Bool
  (case l1
    [(CONSTYPE t1 rest1)
     (case l2
       [(CONSTYPE t2 rest2)
        (if (type-equal? t1 t2)
	    (type-equal-list? rest1 rest2)
	    False)]
       [(NULLTYPE) False])]
    [(NULLTYPE)
     (case l2
       [(CONSTYPE t2 rest2) False]
       [(NULLTYPE)          True])]))
    
(define (type-equal? [t1 : Type] [t2 : Type]) : Bool
  (case t1
    [(NullT)
     (case t2
       [(NullT) True]
       [(Int_) False]
       [(Bool_) False]
       [(Lamt pt bt) False]
       [(Fail) False])]
    [(Int_)
     (case t2
       [(Int_) True]
       [(Bool_) False]
       [(NullT) False]
       [(Lamt pt bt) False]
       [(Fail) False])]
    [(Bool_)
     (case t2
       [(Bool_) True]
       [(Int_) False]
       [(NullT) False]
       [(Lamt pt bt) False]
       [(Fail) False])]
    [(Lamt pt bt)
     (case t2
       [(Lamt pt2 bt2)
        (and
         (type-equal-list? pt pt2)
         (type-equal? bt bt2))]
       [(Int_) False]
       [(Bool_) False]
       [(NullT) False]
       [(Fail) False])]
    [(Fail) False]))
        
(define (params-args-equal? [ptypes : ListType] [args : ListExpr] [a : Arena] [env : (SymDict Type)]) : Bool
  (case ptypes
    [(NULLTYPE)
     (case args
      [(CONSEXPR e rest) False]
      [(NULLEXPR) True])]
    [(CONSTYPE t rest)
     (case args
       [(CONSEXPR e rest2)
        (if (type-equal? t (typecheck e a env))
	    (params-args-equal? rest rest2 a env)
	    False)]
       [(NULLEXPR) False])]))

(define (getParamTypes [params : ListParam]) : ListType
  (case params
    [(CONSPARAM param rest)
      (let ([t : Type (getInnerParamType param)])
        (CONSTYPE t (getParamTypes rest)))]
    [(NULLPARAM)
     (NULLTYPE)]))

(define (getInnerParamType [param : Param]) : Type
  (case param
    [(P e t) t]))

(define (typecheck [expr : Expr] [a : Arena] [env : (SymDict Type)] ): Type
  (case expr
    [(Null)
     (NullT)]
    [(S sym)
     (lookup-env a env sym)]
    [(N n)
     (Int_)]
    [(B b)
     (Bool_)]
    [(Begin ls)
     (typecheck-begin ls a env)]
    [(Lam params body)
     (Lamt (getParamTypes params)
     	   (typecheck body a (lam-extend-env params a env)))]
    [(App lam args)
     (case (typecheck lam a env)
       [(Lamt ptypes btype)
        (if (params-args-equal? ptypes args a env)
            btype
            (Fail))])]))

(define (typecheck-expr [expr : Expr]) : Type
  (letarena a (typecheck expr a (ann (empty-dict a) (SymDict Type)))))

(define (test-typecheck [e : Expr]) : Int
  (case (typecheck-expr e)
    [(Int_) 0]
    [(Bool_) 1]
    [(NullT) 2]
    [(Lamt lt t) 3]
    [(Fail) 4]))

