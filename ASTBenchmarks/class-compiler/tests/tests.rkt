#lang racket

(require "test-uniqify.rkt")
(require "test-flatten.rkt")
(require "test-compiler.rkt")
(require rackunit)

;; TODO: write a parser for R0 so that we can re-use all tests from the class

(define tests
  (list uniqify-tests
        flatten-tests
        compiler-tests))

(define (run-tests)
  (let* ([test-results (flatten (map run-test tests))]
         [failures (filter (compose not test-success?) test-results)]
         [passed (filter test-success? test-results)])
    (if (empty? failures)
        (printf "All ~a tests passed.\n" (length passed))
        (printf "~s tests failed.\n~a"
                (length failures)
                (string-join (map test-result-test-case-name failures) "\n")))))

(run-tests)
