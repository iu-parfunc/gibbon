#! /usr/bin/env racket
#lang typed/racket

;; Run countnode on a SINGLE input file for a given symbol and num iterations.

(require "../../common/racket/parse.rkt"
         "countnodes.gib"
         (only-in "../../grammar_racket.gib" Toplvl))

(define-values (file iters)
  (match (current-command-line-arguments)
    [(vector f i) (values f  (cast (string->number i) Real))]
    [args (error "unexpected number of command line arguments, expected <file> <iterations>, got:\n"
                 args)]))

;; copied exactly + type annotations
(printf "\n\nBenchmark: Counting nodes in file ~a for ~a iterations...\n" file iters)
(printf "============================================================\n")

(define ast : Toplvl
   (time (parse (read (open-input-file file)))))
(printf "Done ingesting AST.\n")

(define-values (cn cpu real gc)
  (time-apply (lambda () (for ([_ (in-range iters)])
                           (countnodes ast)))
              '()))

(define batchseconds (/ real 1000.0))
(printf "ITERS: ~a\n" iters)
(printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
(printf "MEANTIME: ~a\n" (/ batchseconds iters))
(printf "Node count: ~a\n" (car cn))
(printf "Done with count nodes pass.\n")
