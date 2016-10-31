#lang s-exp "../../TreeLang/treelang.rkt"

(require "typechecker.rkt")

(typecheck-expr (N 5))

(typecheck-expr (B #f))

(typecheck-expr (Begin (list (N 5) (N 6) (B #t))))

(typecheck-expr (Lam (list (P (S 'g) (Bool_))) (B #t)))

(typecheck-expr (App (Lam (list (P (S 'g) (Bool_))) (S 'g)) (list (B #t))))
