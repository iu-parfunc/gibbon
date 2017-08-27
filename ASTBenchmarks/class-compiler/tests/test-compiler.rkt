#lang racket

(require "../compiler.gib")
(require "../common.gib")

(require rackunit)

(provide compiler-tests)

(define-test-suite compiler-tests
  (compiler-test1))

(define (compiler-test1)
  (test-equal? "compiler-tests:1"
               (compile
                (ProgramR (LetER (vector 'a (LitER 2))
                                 (AddER (LitER 40) (VarER 'a)))))

               42))
