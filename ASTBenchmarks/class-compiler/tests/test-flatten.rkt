#lang racket

(require "../passes/flatten.gib")
(require "../common.gib")

(require rackunit)

(provide flatten-tests)

(define-test-suite flatten-tests
  (flatten-test1)
  (flatten-test2)
  (flatten-test3)
  (flatten-test4)
  (flatten-test5)
  (flatten-test6)
  (flatten-test7))

(define (flatten-test1)
  (test-equal? "flatten-tests:1"
               (flatten
                (ProgramR (LetER (vector 'a1 (LitER 2))
                                 (AddER (LitER 40) (VarER 'a1)))))

               (ProgramC
                (ConsSym 'a1 (ConsSym 'tmp1235 (NullSyms)))
                (ConsStmt
                 (AssignE 'a1 (LitEC 2))
                 (ConsStmt
                  (AssignE 'tmp1235 (AddEC (LitEC 40) (VarEC 'a1)))
                  (ConsStmt (RetE (VarEC 'tmp1235)) (NullStmts)))))))

(define (flatten-test2)
  (test-equal? "flatten-tests:2"
               (flatten
                (ProgramR (LetER (vector 'a1 (LitER 2))
                                 (LetER (vector 'b2 (LitER 40))
                                        (AddER (VarER 'a1) (VarER 'b2))))))

               (ProgramC
                (ConsSym 'a1 (ConsSym 'b2 (ConsSym 'tmp1235 (NullSyms))))
                (ConsStmt
                 (AssignE 'a1 (LitEC 2))
                 (ConsStmt
                  (AssignE 'b2 (LitEC 40))
                  (ConsStmt
                   (AssignE 'tmp1235 (AddEC (VarEC 'a1) (VarEC 'b2)))
                   (ConsStmt (RetE (VarEC 'tmp1235)) (NullStmts))))))))

(define (flatten-test3)
  (test-equal? "flatten-tests:3"
               (flatten
                (ProgramR (LetER (vector 'a1 (LitER 2))
                                 (LetER (vector 'a2 (LitER 40))
                                        (AddER (VarER 'a2) (VarER 'a2))))))

               (ProgramC
                (ConsSym 'a1 (ConsSym 'a2 (ConsSym 'tmp1235 (NullSyms))))
                (ConsStmt
                 (AssignE 'a1 (LitEC 2))
                 (ConsStmt
                  (AssignE 'a2 (LitEC 40))
                  (ConsStmt
                   (AssignE 'tmp1235 (AddEC (VarEC 'a2) (VarEC 'a2)))
                   (ConsStmt (RetE (VarEC 'tmp1235)) (NullStmts))))))))

(define (flatten-test4)
  (test-equal? "flatten-tests:4"
               (flatten
                (ProgramR (LetER (vector 'x1 (LitER 2))
                                 (AddER (LetER (vector 'x2 (LitER 40))
                                               (VarER 'x2))
                                        (VarER 'x1)))))

               (ProgramC
                (ConsSym 'x1 (ConsSym 'x2 (ConsSym 'tmp1235 (NullSyms))))
                (ConsStmt
                 (AssignE 'x1 (LitEC 2))
                 (ConsStmt
                  (AssignE 'x2 (LitEC 40))
                  (ConsStmt
                   (AssignE 'tmp1235 (AddEC (VarEC 'x2) (VarEC 'x1)))
                   (ConsStmt (RetE (VarEC 'tmp1235)) (NullStmts))))))))

(define (flatten-test5)
  (test-equal? "flatten-tests:5"
               (flatten
                (ProgramR (LetER (vector 'a1 (LitER 42))
                                 (LetER (vector 'b2 (VarER 'a1))
                                        (VarER 'b2)))))
               (ProgramC
                (ConsSym 'a1 (ConsSym 'b2 (NullSyms)))
                (ConsStmt
                 (AssignE 'a1 (LitEC 42))
                 (ConsStmt
                  (AssignE 'b2 (VarEC 'a1))
                  (ConsStmt (RetE (VarEC 'b2)) (NullStmts)))))))

(define (flatten-test6)
  (test-equal? "flatten-tests:6"
               (flatten
                (ProgramR (AddER (LitER 52) (NegER (LitER 10)))))

               (ProgramC
                (ConsSym 'tmp1235 (ConsSym 'tmp1236 (NullSyms)))
                (ConsStmt
                 (AssignE 'tmp1235 (NegEC (LitEC 10)))
                 (ConsStmt
                  (AssignE 'tmp1236 (AddEC (LitEC 52) (VarEC 'tmp1235)))
                  (ConsStmt (RetE (VarEC 'tmp1236)) (NullStmts)))))))

(define (flatten-test7)
  (test-equal? "flatten-tests:7"
               (flatten
                (ProgramR (LetER (vector 'x1 (AddER (NegER (LitER 10)) (LitER 11)))
                                 (AddER (VarER 'x1) (LitER 41)))))

               (ProgramC
                (ConsSym 'tmp1234
                         (ConsSym 'tmp1236 (ConsSym 'x1 (ConsSym 'tmp1238 (NullSyms)))))
                (ConsStmt
                 (AssignE 'tmp1234 (NegEC (LitEC 10)))
                 (ConsStmt
                  (AssignE 'tmp1236 (AddEC (VarEC 'tmp1234) (LitEC 11)))
                  (ConsStmt
                   (AssignE 'x1 (VarEC 'tmp1236))
                   (ConsStmt
                    (AssignE 'tmp1238 (AddEC (VarEC 'x1) (LitEC 41)))
                    (ConsStmt (RetE (VarEC 'tmp1238)) (NullStmts)))))))))
