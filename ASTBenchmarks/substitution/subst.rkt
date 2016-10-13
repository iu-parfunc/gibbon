#! /usr/bin/env racket
#lang racket

(require racket/match racket/trace)

(define args (current-command-line-arguments))
(define-values (oldsym file iters)
  (match args
    [(vector o f i) (values (string->symbol o) f (string->number i))]
    [else (error "unexpected number of command line arguments, expected <symbol> <file> <iterations>, got:\n"
                 args)]))

(printf "\n\nBenchmark: Substituting symbol ~a in file ~a for ~a iterations...\n" oldsym file iters)
(printf "============================================================\n")

;; Abstract over some weird variation in the expanded output.
;; ----------------------------------------------------------
(define (module-begin? s) (or (eq? s '#%plain-module-begin)
                              (eq? s '#%module-begin)))
(define (lambda? s)     (or (eq? s 'lambda) (eq? s '#%plain-lambda)))
(define (app? s)        (or (eq? s '#%app) (eq? s '#%plain-app)))
;; ----------------------------------------------------------
(define (module-sym? s) (or (eq? s 'module) (eq? s 'module*)))
(define (let-sym? s)    (or (eq? s 'let-values) (eq? s 'letrec-values)))

(define (bound-in? sym formals)
  (match formals
    [(? symbol?) (eq? sym formals)]
    [`(,(? symbol? f*) ...)
     (or (eq? sym rest) (memq sym f*))]
    [`(,(? symbol? f*) ... . ,(? symbol? rest))
     (or (eq? sym rest) (memq sym f*))]
    ))

(define (subst old new e0)  
  (#|trace-|#define (top e) ; top-level-form 
    (match e
      [`(#%expression ,e) `(#%expression ,(expr e))]
      [`(module ,id ,path (,(? module-begin? mb) ,m* ...))
       `(module ,id ,path (,mb ,@(map mlf m*)))]
      [`(begin ,top-level-form* ...)            `(begin            ,@(map top top-level-form*))]
      [`(begin-for-syntax ,top-level-form* ...) `(begin-for-syntax ,@(map top top-level-form*))]
      [else (gtlf e)] ;; general-top-level-form
      ))

  (#|trace-|#define (mlf e) ; module-level-form
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
  
  (#|trace-|#define (expr e)
    (match e
      ;; Variable references:
      [(? symbol?) (if (eq? oldsym e) newsym e)]
      [`(#%top . ,id)              (if (eq? oldsym id) `(#%top . ,newsym) e)]
      [`(#%variable-reference ,id) (if (eq? oldsym id) `(#%variable-reference ,newsym) e)]
      [`(#%variable-reference (#%top . ,id)) (if (eq? oldsym id) `(#%variable-reference (#%top . ,newsym)) e)]

      ;; Leaf forms:
      ['(#%variable-reference) e]
      [`(quote ,_) e]
      [`(quote-syntax . ,_) e]

      ;; Binding forms:
      [`(,(? lambda? lam) ,formals ,bods ...)
       `(,lam ,formals
          ,(if (bound-in? oldsym formals)
               bods (map expr bods)))]
      [`(case-lambda . ,cases)
       `(case-lambda
          ,(for/list ((c cases))
             (match-let ([`(,formals ,e* ...) c])
                (if (bound-in? oldsym formals) c
                    `(,formals ,@(map expr e*))))))]
      [`(,(? let-sym? lett) ,binds ,bod* ...)
       `(,lett ,(for/list ((b binds))
                       (match-let ([`[(,id* ...) ,rhs] b])
                         (if (bound-in? oldsym id*) b
                             `[,id* ,(expr rhs)])))
               ,@(map expr bod*))]

      ;; Other:
      [`(if ,a ,b ,c) `(if ,(expr a) ,(expr b) ,(expr c))]
      [`(begin ,e* ...) `(begin ,@(map expr e*))]
      [`(begin0 ,e1 ,e* ...) `(begin0 ,(expr e1) ,@(map expr e*))]
      [`(,(? app? app) ,e* ...) `(,app ,@(map expr e*))]
      [`(set! ,id ,e) `(set! ,id ,(expr e))]
      [`(with-continuation-mark ,a ,b ,c) `(with-continuation-mark ,(expr a) ,(expr b) ,(expr c))]

      ;; HACK: this should be disallowed by the grammar.  But we ran
      ;; across it.  For instance in:
      ;;    ../expanded_racket/racket/collects/setup/parallel-do.rkt.out.sexp
      [`(#%expression ,e) `(#%expression ,(expr e))]
      ))
  
  (#|trace-|#define (gtlf e)
    (match e
      [`(define-values (,id* ...) ,e)
       `(define-values ,id* ,(expr e))]
      [`(define-syntaxes (,id* ...) ,e)
       `(define-syntaxes ,id* ,(expr e))]
      [`(#%require ,raw-require-spec* ...)
       `(#%require ,@raw-require-spec*)] ;; abstract for now.
      [else (expr e)]
      ))
  ;; Kick things off:
  (top e0))


(define ast (time (read (open-input-file file))))
(printf "Done ingesting AST.\n")

(define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
(time (for ((i (range iters)))
        (subst oldsym newsym ast)))
