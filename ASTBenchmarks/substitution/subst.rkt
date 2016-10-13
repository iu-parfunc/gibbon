#! /usr/bin/env racket
#lang racket

(require racket/match
         racket/trace)

(define args (current-command-line-arguments))
(define-values (oldsym file iters)
  (match args
    [(vector o f i) (values (string->symbol o) f (string->number i))]
    [else (error "unexpected number of command line arguments, expected <symbol> <file> <iterations>, got:\n"
                 args)]))

(printf "Benchmark: Substituting symbol ~a in file ~a for ~a iterations...\n" oldsym file iters)

;; Abstract over some weird variation in the expanded output.
;; ----------------------------------------------------------
(define (module-begin? s) (or (eq? s '#%plain-module-begin)
                              (eq? s '#%module-begin)))
(define (module-sym? s) (or (eq? s 'module) (eq? s 'module*)))
(define (lambda? s)     (or (eq? s 'lambda) (eq? s '#%plain-lambda)))
(define (app? s)        (or (eq? s '#%app) (eq? s '#%plain-app)))
;; ----------------------------------------------------------


(define (subst old new e0)  
  (trace-define (top e) ; top-level-form 
    (match e
      [`(#%expression ,e) `(#%expression ,(expr e))]
      [`(module ,id ,path (,(? module-begin? mb) ,m* ...))
       `(module ,id ,path (,mb ,@(map mlf m*)))]
      [`(begin ,top-level-form* ...)            `(begin            ,@(map top top-level-form*))]
      [`(begin-for-syntax ,top-level-form* ...) `(begin-for-syntax ,@(map top top-level-form*))]
      [else (gtlf e)] ;; general-top-level-form
      ))

  (trace-define (mlf e) ; module-level-form
    (match e 
      [`(#%provide ,r* ...) `(#%provide ,*(map rps r*))]
      [`(begin-for-syntax ,m* ...) `(begin-for-syntax ,@(map mlf m*))]
      [`(#%declare ,declaration-keyword* ...)
       `(#%declare ,@declaration-keyword*)]
      ;; submodule forms:
      [`(,(? module-sym? mod) ,id ,path-or-false (,(? module-begin? mb) ,m* ...))
       `(,mod ,id ,path-or-false (,mb ,@(map mlf m*)))]
      [else (gtlf e)] ;; general-top-level-form
      ))
  
  ;; ; raw-provide-spec: No processing here:
  (define (rps e) e)
  
  (trace-define (expr e)
    (match e
      [(? symbol?) (if (eq? oldsym e) newsym e)]
      [`(,(? lambda? lam) ,formals ,bods ...)
       `(,lam ,formals
          ,(if (memq oldsym formals)
               bods (map expr bods)))]
      [`(if ,a ,b ,c) `(if ,(expr a) ,(expr b) ,(expr c))]
      [`(begin ,e* ...) `(begin ,@(map expr e*))]
      [`(begin0 ,e1 ,e* ...) `(begin0 ,(expr e1) ,@(map expr e*))]
      [`(,(? app? app) ,e* ...) `(,app ,@(map expr e*))]
      [`(quote ,datum) e]

;;  	 	|	 	(case-lambda (formals expr ...+) ...)
;; (let-values ([(id ...) expr] ...)
;;   expr ...+)
;; (letrec-values ([(id ...) expr] ...)
;;   expr ...+)
;;  	 	|	 	(set! id expr)
;;  	 	|	 	
;;  	 	|	 	(quote-syntax datum)
;;  	 	|	 	(quote-syntax datum #:local)
;;  	 	|	 	(with-continuation-mark expr expr expr)
;;  	 	|	 	(#%top . id)
;;  	 	|	 	(#%variable-reference id)
;;  	 	|	 	(#%variable-reference (#%top . id))
;;  	 	|	 	(#%variable-reference)
 	 	 	 	 
;;   formals	 	=	 	(id ...)
;;  	 	|	 	(id ...+ . id)
;;  	 	|	 	id


      ))

  (trace-define (gtlf e)
    (match e
      [`(define-values (,id* ...) ,e)
       `(define-values ,id* ,(expr e))]
      [`(define-syntaxes (,id* ...) ,e)
       `(define-syntaxes ,id* ,(expr e))]
      [`(#%require ,raw-require-spec* ...)
       `(#%require ,@raw-require-spec*)] ;; abstract for now.
      [else (expr e)]
      ))
  
  (top e0))

(define ast (time (read (open-input-file file))))
(printf "Done ingesting AST.\n")


(define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
(time (for ((i (range iters)))
        (subst oldsym newsym ast)))
