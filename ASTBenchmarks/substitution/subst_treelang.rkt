#! /usr/bin/env racket
#lang s-exp "../../TreeLang/treelang.rkt"

(require "../grammar_racket.sexp"
         (only-in typed/racket define-namespace-anchor namespace-anchor->namespace
                  eval call-with-values lambda match ... quasiquote unquote symbol?
                  map exact-integer? -> Any andmap list? integer?
                  define-values current-command-line-arguments
         values string->symbol string->number printf read open-input-file
         string-append symbol->string for in-range cast Real))

;; copied exactly
(define-values (oldsym file iters)
  (match (current-command-line-arguments)
    [(vector o f i) (values (string->symbol o) f (string->number i))]
    [args (error "unexpected number of command line arguments, expected <symbol> <file> <iterations>, got:\n"
                 args)]))

(define (subst [old : Sym] [new : Sym] [e0 : Toplvl]) : Toplvl
  (top old new e0))

#|
(data Toplvl
      [DefineValues   (Listof Sym) Expr]
      [DefineSyntaxes (Listof Sym) Expr]
      [Expression Expr])
|#
(define (top [old : Sym] [new : Sym] [e : Toplvl]) : Toplvl
  (case e
    [(DefineValues ls e)
     (DefineValues ls (expr old new e))]
    [(DefineSyntaxes ls e)
     (DefineSyntaxes ls (expr old new e))]
    [(BeginTop ls)
     (BeginTop (for/list ([t : Toplvl ls])
                 (top old new t)))]
    [(Expression e)
     (Expression (expr old new e))]))

(define (expr [old : Sym] [new : Sym] [e : Expr]) : Expr
  (case e
    ;; Variable references:
    [(VARREF s)
     (if (eq? old s)
         (VARREF new)
         e)]
    [(Top s)
     (if (eq? old s)
         (Top new)
         e)]
    [(VariableReference s)   ; #%variable-reference
     (if (eq? old s)
         (VariableReference new)
         e)] 
    [(VariableReferenceTop s)   ; #%variable-reference (#%top . id)
     (if (eq? old s)
         (VariableReferenceTop new)
         e)]

    ;; Leaf forms:
    [(VariableReferenceNull)     ; (#%variable-reference)
     e]
    [(Quote d) e]
    [(QuoteSyntax d) e]
    [(QuoteSyntaxLocal d) e] ;; (quote-syntax datum #:local)

    ;; Binding forms:
    [(Lambda formals lse)
     (Lambda formals (if (bound-in? old formals)
                         lse
                         (for/list ([e : Expr lse])
                           (expr old new e))))]
    [(CaseLambda cases)
     (CaseLambda (for/list ([c : LAMBDACASE cases])
                   (case c
                     [(MKLAMBDACASE formals exprs)
                      (if (bound-in? old formals)
                          c
                          (MKLAMBDACASE formals (for/list ([e : Expr exprs])
                                                  (expr old new e))))])))]
    [(LetValues binds body)
     (LetValues (for/list ([b : LVBIND binds])
                  (case b
                    [(MKLVBIND syms e)
                     (if (helper old syms)
                         b
                         (MKLVBIND syms (expr old new e)))]))

                (for/list ([e : Expr body])
                  (expr old new e)))]
    [(LetrecValues binds body)
     (LetrecValues (for/list ([b : LVBIND binds])
                     (case b
                       [(MKLVBIND syms e)
                        (if (helper old syms)
                            b
                            (MKLVBIND syms (expr old new e)))]))
                   
                   (for/list ([e : Expr body])
                     (expr old new e)))]
    [(If cond then else)
     (If (expr old new cond) (expr old new then) (expr old new else))]
    [(Begin exprs)
     (Begin (for/list ([e : Expr exprs])
              (expr old new e)))]
    [(Begin0 e1 exprs)
     (Begin0 (expr old new e1) (for/list ([e : Expr exprs])
                                 (expr old new e)))]
    [(App exprs)  ;; (#%plain-app expr ...+)
     (App (for/list ([e : Expr exprs])
            (expr old new e)))]
    [(SetBang s e)
     (SetBang s (expr old new e))]
    [(WithContinuationMark e1 e2 e3)
     (WithContinuationMark (expr old new e1) (expr old new e2) (expr old new e3))]))

(define (bound-in? [sym : Sym] [formals : Formals]) : Bool
  (case formals
    [(F1 syms)
     (helper sym syms)]
    [(F2 syms s)
     (if (eq? sym s)
         #t
         (helper sym syms))]
    [(F3 s)
     (eq? sym s)]))

(define (helper [sym : Sym] [ls : (Listof Sym)]) : Bool
  (if (empty? ls)
      #f
      (if (eq? (car ls) sym)
          #t
          (helper sym (cdr ls)))))

;; copied exactly + type annotations
(printf "\n\nBenchmark: Substituting symbol ~a in file ~a for ~a iterations...\n" oldsym file iters)
(printf "============================================================\n")

(: parse : (Any -> Toplvl))
(define (parse v)
  (match v
    [`(DefineValues (,x ...) ,(app parse-expr e))
     #:when  (andmap symbol? x)
     (DefineValues x e)]
    [`(DefineSyntaxes (,x ...) ,(app parse-expr e))
     #:when (andmap symbol? x)
     (DefineSyntaxes x e)]
    [`(BeginTop ,e ...) (BeginTop (map parse e))]
    [`(Expression ,(app parse-expr e))
     (Expression e)]))

(: parse-formals : (Any -> Formals))
(define (parse-formals v)
  (match v
    [`(F1 (,x ...))
     #:when (andmap symbol? x)
     (F1 x)]
    [`(F2 (,x ...) ,(? symbol? y))
     #:when (andmap symbol? x)
     (F2 x y)]
    [`(F3 ,(? symbol? x))
     (F3 x)]))

(: parse-lambdacase : (Any -> LAMBDACASE))
(define (parse-lambdacase v)
  (match v
    [`(MKLAMBDACASE ,(app parse-formals f) ,e ...)
     (MKLAMBDACASE f (map parse-expr e))]))

(: parse-lvbind : (Any -> LVBIND))
(define (parse-lvbind v)
  (match v
    [`(MKLVBIND (,x ...) ,(app parse-expr e))
     #:when (andmap symbol? x)
     (MKLVBIND x e)]))

(: parse-datum : (Any -> Datum))
(define (parse-datum v)
  (match v
    [`(INTLIT ,(? integer? i))
     (INTLIT i)]))

(: parse-expr : (Any -> Expr))
(define (parse-expr v)
  (match v
    [`(VARREF ,(? symbol? x)) (VARREF x)]
    [`(Lambda ,(app parse-formals fs) ,e ...)
     (Lambda fs (map parse-expr e))]
    [`(CaseLambda ,lc ...)
     (CaseLambda (map parse-lambdacase lc))]
    [`(If ,(app parse-expr cond) ,(app parse-expr then) ,(app parse-expr else))
     (If cond then else)]
    [`(Begin ,e ...)
     (Begin (map parse-expr e))]
    [`(Begin0 ,(app parse-expr e1) ,e ...)
     (Begin0 e1 (map parse-expr e))]
    [`(LetValues (,lvs ...) ,e ...)
     (LetValues (map parse-lvbind lvs) (map parse-expr e))]
    [`(LetrecValues (,lvs ...) ,e ...)
     (LetrecValues (map parse-lvbind lvs) (map parse-expr e))]
    [`(SetBang ,(? symbol? x) ,(app parse-expr e))
     (SetBang x e)]
    [`(Quote ,(app parse-datum d))
     (Quote d)]
    [`(QuoteSyntax ,(app parse-datum d))
     (QuoteSyntax d)]
    [`(WithContinuationMark ,(app parse-expr e1) ,(app parse-expr e2) ,(app parse-expr e3))
     (WithContinuationMark e1 e2 e3)]
    [`(App ,e ...)
     (App (map parse-expr e))]
    [`(Top ,(? symbol? x))
     (Top x)]
    [`(VariableReference ,(? symbol? x))
     (VariableReference x)]
    [`(VariableReferenceTop ,(? symbol? x))
     (VariableReferenceTop x)]
    [`(VariableReferenceNull)
     (VariableReferenceNull)]))


(define ast : Toplvl
   (time (parse (read (open-input-file file)))))
(printf "Done ingesting AST.\n")

(define newsym (string->symbol (string-append (symbol->string oldsym) "99")))
(time (for ([_ (in-range (cast iters Real))])
        (subst oldsym newsym ast)))
(printf "Done with substitution pass.\n")



