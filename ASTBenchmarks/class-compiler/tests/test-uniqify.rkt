#lang racket

(require "../passes/uniqify.gib")
(require "../common.gib")

(require rackunit)

(provide uniqify-tests)

(define-test-suite uniqify-tests
  (uniqify-test1)
  (uniqify-test2)
  (uniqify-test3)
  (uniqify-test4))

(define (uniqify-test1)
  (test-equal? "uniqify-tests:1"
               (uniqify
                (ProgramR (LetER (vector 'a (LitER 2))
                                 (AddER (LitER 40) (VarER 'a)))))

               (ProgramR (LetER (vector 'a1 (LitER 2))
                                (AddER (LitER 40) (VarER 'a1))))))

(define (uniqify-test2)
  (test-equal? "uniqify-tests:2"
               (uniqify
                (ProgramR (LetER (vector 'a (LitER 2))
                                 (LetER (vector 'b (LitER 40))
                                        (AddER (VarER 'a) (VarER 'b))))))

               (ProgramR (LetER (vector 'a1 (LitER 2))
                                (LetER (vector 'b2 (LitER 40))
                                       (AddER (VarER 'a1) (VarER 'b2)))))))

(define (uniqify-test3)
  (test-equal? "uniqify-tests:3"
               (uniqify
                (ProgramR (LetER (vector 'a (LitER 2))
                                 (LetER (vector 'a (LitER 40))
                                        (AddER (VarER 'a) (VarER 'a))))))

               (ProgramR (LetER (vector 'a1 (LitER 2))
                                (LetER (vector 'a2 (LitER 40))
                                       (AddER (VarER 'a2) (VarER 'a2)))))))

;; from the book
(define (uniqify-test4)
  (test-equal? "uniqify-tests:4"
               (uniqify
                (ProgramR (LetER (vector 'x (LitER 2))
                                 (AddER (LetER (vector 'x (LitER 40))
                                               (VarER 'x)) (VarER 'x)))))

               (ProgramR
                (LetER (vector 'x1 (LitER 2))
                       (AddER [LetER (vector 'x2 (LitER 40))
                                     (VarER 'x2)]
                              (VarER 'x1))))))
