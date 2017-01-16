#! /usr/bin/env racket
#lang typed/racket/base

(require (only-in "../../common/racket/parse.rkt" parse)
         "treewalk.rkt"
         racket/cmdline
         racket/file racket/match
         (only-in "../../grammar_racket.sexp" Toplvl))


(define-values (oldsym file iters)
  (match (current-command-line-arguments)
    [(vector o f i) (values (string->symbol o) f 
                            (cast (string->number i) Real))]
    [args (error "unexpected number of command line arguments, expected <symbol> <file> <iterations>, got:\n"
                 args)]))

#;
(match-define (cons file iters)
  (command-line #:args (#{f : String} #{i : String})
                (cons f (cast (string->number i) Real))))

;; copied exactly + type annotations
(printf "\n\nBenchmark: Tree walk in file ~a for ~a iterations...\n"
        file iters)
(printf "============================================================\n")

(define ast : Toplvl
   (time (parse (read (open-input-file file)))))
(printf "Done ingesting AST.\n")

(define-values (_ cpu real gc)
  (time-apply (lambda () (for ([_ (in-range iters)])
                           (treewalk ast)))
              '()))

(define batchseconds (/ real 1000.0))
(printf "ITERS: ~a\n" iters)
(printf "BATCHTIME: ~a\n" (exact->inexact batchseconds))
(printf "MEANTIME: ~a\n" (/ batchseconds iters))
(printf "Done with copy propogation pass.\n")
