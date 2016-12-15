#lang s-exp "../gibbon/main.rkt"

(provide DefineValues DefineSyntaxes Expression VARREF Lambda
         CaseLambda If Begin Begin0 LetValues LetrecValues SetBang
         Quote QuoteSyntax QuoteSyntaxLocal WithContinuationMark App
         Top VariableReference VariableReferenceTop VariableReferenceNull
         BeginTop Datum
         LVBIND CONSLVBIND NULLLVBIND
         LAMBDACASE CONSLAMBDACASE NULLLAMBDACASE
         INTLIT  Expr Toplvl
         Formals F1 F2 F3 
         ListExpr CONSEXPR NULLEXPR
         ListSym  CONSSYM  NULLSYM
         ListToplvl CONSTOPLVL NULLTOPLVL
         pack-Toplvl)

(data Toplvl
      [DefineValues   ListSym Expr]
      [DefineSyntaxes ListSym Expr]
      [BeginTop ListToplvl]
      [Expression Expr])

(data Expr
      (VARREF Sym)              ;; Tag 0
      (Lambda Formals ListExpr)	;; Tag 1
      (CaseLambda LAMBDACASE)
      (If Expr Expr Expr)
      (Begin ListExpr)
      (Begin0 Expr ListExpr)
      (LetValues    LVBIND ListExpr)
      (LetrecValues LVBIND ListExpr)
      (SetBang Sym Expr)
      (Quote Datum)
      (QuoteSyntax Datum)
      (QuoteSyntaxLocal Datum)  ;; (quote-syntax datum #:local)
      (WithContinuationMark Expr Expr Expr)
      (App Expr ListExpr) ;; (#%plain-app expr ...+)
      (Top Sym)
      (VariableReference    Sym) ; #%variable-reference
      (VariableReferenceTop Sym) ; #%variable-reference (#%top . id)
      (VariableReferenceNull)    ; (#%variable-reference)
      )

(data LVBIND
      (CONSLVBIND ListSym Expr LVBIND)
      (NULLLVBIND ))

(data LAMBDACASE
      (CONSLAMBDACASE Formals ListExpr LAMBDACASE)  ;; (formals expr ...+) 
      (NULLLAMBDACASE ))

;; RRN: How far do we need to go here?
(data Datum
      (INTLIT Int)
      ; (SYMLIT Sym)
      ; (CONS Datum Datum)
      ; (NULL)
      )

(data Formals
      [F1 ListSym]        ;; Tag 0
      [F2 ListSym Sym]    ;; Tag 1
      [F3 Sym])                ;; Tag 2

(data ListToplvl
      (CONSTOPLVL Toplvl ListToplvl)
      (NULLTOPLVL))

(data ListExpr
      (CONSEXPR Expr ListExpr)
      (NULLEXPR))

(data ListSym
      (CONSSYM Sym ListSym)
      (NULLSYM))

;; ================================================================================
;; Cached copy of the grammar from:
;; 
;;     https://docs.racket-lang.org/reference/syntax-model.html
;; 
;; top-level-form
;;                 =	 	general-top-level-form
;;  	 	|	 	(#%expression expr)
;;                 |               (module id module-path
;;                                   (#%plain-module-begin
;;                                    module-level-form ...))
;;  	 	|	 	(begin top-level-form ...)
;;  	 	|	 	(begin-for-syntax top-level-form ...)
 	 	 	 	 
;;   module-level-form	 	=	 	general-top-level-form
;;  	 	|	 	(#%provide raw-provide-spec ...)
;;  	 	|	 	(begin-for-syntax module-level-form ...)
;;  	 	|	 	submodule-form
;;  	 	|	 	(#%declare declaration-keyword ...)
 	 	 	 	 
;;   submodule-form	 	=	 	
;;                                    (module id module-path
;;                                      (#%plain-module-begin
;;                                       module-level-form ...))
;;                |	 	
;;                                    (module* id module-path
;;                                      (#%plain-module-begin
;;                                       module-level-form ...))
;;                |	 	
;;                                    (module* id #f
;;                                      (#%plain-module-begin
;;                                       module-level-form ...))
 	 	 	 	 
;;   general-top-level-form  =  expr
;;  	 	|	 	(define-values (id ...) expr)
;;  	 	|	 	(define-syntaxes (id ...) expr)
;;  	 	|	 	(#%require raw-require-spec ...)
 	 	 	 	 
;;   expr	 	=	 	id
;;  	 	|	 	(#%plain-lambda formals expr ...+)
;;  	 	|	 	(case-lambda (formals expr ...+) ...)
;;  	 	|	 	(if expr expr expr)
;;  	 	|	 	(begin expr ...+)
;;  	 	|	 	(begin0 expr expr ...)
;;  	 	|	 	
;; (let-values ([(id ...) expr] ...)
;;   expr ...+)
;;  	 	|	 	
;; (letrec-values ([(id ...) expr] ...)
;;   expr ...+)
;;  	 	|	 	(set! id expr)
;;  	 	|	 	(quote datum)
;;  	 	|	 	(quote-syntax datum)
;;  	 	|	 	(quote-syntax datum #:local)
;;  	 	|	 	(with-continuation-mark expr expr expr)
;;  	 	|	 	(#%plain-app expr ...+)
;;  	 	|	 	(#%top . id)
;;  	 	|	 	(#%variable-reference id)
;;  	 	|	 	(#%variable-reference (#%top . id))
;;  	 	|	 	(#%variable-reference)
 	 	 	 	 
;;   formals	 	=	 	(id ...)
;;  	 	|	 	(id ...+ . id)
;;  	 	|	 	id

