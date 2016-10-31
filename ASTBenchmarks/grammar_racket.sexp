#lang s-exp "../TreeLang/treelang.rkt"

(provide MKPROG DefineValues DefineSyntaxes Expression VARREF Lambda
         CaseLambda If Begin Begin0 LetValues LetrecValues SetBang
         Quote QuoteSyntax QuoteSyntaxLocal WithContinuationMark App
         Top VariableReference VariableReferenceTop VariableReferenceNull
         MKLVBIND MKLAMBDACASE INTLIT F1 F2 F3 Expr Toplvl Formals LVBIND LAMBDACASE Prog)

(data Prog [MKPROG (Listof Toplvl)])

(data Toplvl
      [DefineValues   (Listof Sym) Expr]
      [DefineSyntaxes (Listof Sym) Expr]
      [BeginTop (Listof Toplvl)]
      [Expression Expr])

(data Expr
      (VARREF Sym)
      (Lambda Formals (Listof Expr))	
      (CaseLambda (Listof LAMBDACASE))
      (If Expr Expr Expr)
      (Begin (Listof Expr))
      (Begin0 Expr (Listof Expr))
      (LetValues   (Listof LVBIND) (Listof Expr))
      (LetrecValues (Listof LVBIND) (Listof Expr))
      (SetBang Sym Expr)
      (Quote Datum)
      (QuoteSyntax Datum)
      (QuoteSyntaxLocal Datum)  ;; (quote-syntax datum #:local)
      (WithContinuationMark Expr Expr Expr)
      (App (Listof Expr)) ;; (#%plain-app expr ...+)
      (Top Sym)
      (VariableReference    Sym) ; #%variable-reference
      (VariableReferenceTop Sym) ; #%variable-reference (#%top . id)
      (VariableReferenceNull)    ; (#%variable-reference)
      )

(data LVBIND (MKLVBIND (Listof Sym) Expr))

(data LAMBDACASE (MKLAMBDACASE Formals (Listof Expr)))  ;; (formals expr ...+) 

;; RRN: How far do we need to go here?
(data Datum
      (INTLIT Int)
      ; (SYMLIT Sym)
      ; (CONS Datum Datum)
      ; (NULL)
      )

(data Formals
      [F1 (Listof Sym)]
      [F2 (Listof Sym) Sym]
      [F3 Sym])



#|
Cached copy of the grammar from:

    https://docs.racket-lang.org/reference/syntax-model.html


top-level-form
                =	 	general-top-level-form
 	 	|	 	(#%expression expr)
                |               (module id module-path
                                  (#%plain-module-begin
                                   module-level-form ...))
 	 	|	 	(begin top-level-form ...)
 	 	|	 	(begin-for-syntax top-level-form ...)
 	 	 	 	 
  module-level-form	 	=	 	general-top-level-form
 	 	|	 	(#%provide raw-provide-spec ...)
 	 	|	 	(begin-for-syntax module-level-form ...)
 	 	|	 	submodule-form
 	 	|	 	(#%declare declaration-keyword ...)
 	 	 	 	 
  submodule-form	 	=	 	
                                   (module id module-path
                                     (#%plain-module-begin
                                      module-level-form ...))
               |	 	
                                   (module* id module-path
                                     (#%plain-module-begin
                                      module-level-form ...))
               |	 	
                                   (module* id #f
                                     (#%plain-module-begin
                                      module-level-form ...))
 	 	 	 	 
  general-top-level-form  =  expr
 	 	|	 	(define-values (id ...) expr)
 	 	|	 	(define-syntaxes (id ...) expr)
 	 	|	 	(#%require raw-require-spec ...)
 	 	 	 	 
  expr	 	=	 	id
 	 	|	 	(#%plain-lambda formals expr ...+)
 	 	|	 	(case-lambda (formals expr ...+) ...)
 	 	|	 	(if expr expr expr)
 	 	|	 	(begin expr ...+)
 	 	|	 	(begin0 expr expr ...)
 	 	|	 	
(let-values ([(id ...) expr] ...)
  expr ...+)
 	 	|	 	
(letrec-values ([(id ...) expr] ...)
  expr ...+)
 	 	|	 	(set! id expr)
 	 	|	 	(quote datum)
 	 	|	 	(quote-syntax datum)
 	 	|	 	(quote-syntax datum #:local)
 	 	|	 	(with-continuation-mark expr expr expr)
 	 	|	 	(#%plain-app expr ...+)
 	 	|	 	(#%top . id)
 	 	|	 	(#%variable-reference id)
 	 	|	 	(#%variable-reference (#%top . id))
 	 	|	 	(#%variable-reference)
 	 	 	 	 
  formals	 	=	 	(id ...)
 	 	|	 	(id ...+ . id)
 	 	|	 	id
|#
