#! /usr/bin/env racket
#lang typed/racket/base

;; Run copy propogation on a SINGLE input file for a given symbol and num iterations.

(require "../../common/racket/parse.rkt"
         "copyprop.rkt"
         racket/cmdline
         racket/file racket/match
         (only-in "../../grammar_racket.sexp" Toplvl))

(match-define (cons file iters)
  (command-line #:args (#{f : String} #{i : String})
                (cons f (cast (string->number i) Real))))

;; copied exactly + type annotations
(printf "\n\nBenchmark: Copy propogation in file ~a for ~a iterations...\n"
        file iters)
(printf "============================================================\n")

(define ast : Toplvl
   (time (parse (read (open-input-file file)))))
(printf "Done ingesting AST.\n")

(define-values (_ cpu real gc)
  (time-apply (lambda () (for ([_ (in-range iters)])
                           (copyprop ast)))
              '()))
(define batchseconds (/ real 1000.0))
(printf "ITERS: ~a\n" iters)
(printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
(printf "MEANTIME: ~a\n" (/ batchseconds iters))
(printf "Done with copy propogation pass.\n")
