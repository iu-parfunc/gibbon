#! /usr/bin/env racket
#lang typed/racket

(require "./subst_treelang.rkt"
         "../../common/racket/command_line_runner.rkt"
         "../../grammar_racket.sexp")

(define oldsym 'call-with-values) ;; Hardcode this, doesn't matter.
(define newsym (string->symbol (string-append (symbol->string oldsym) "99")))

(define (iterate-subst [ast : Toplvl] [n : Integer]) : Toplvl
  (define out (subst oldsym newsym ast))
  (for ((i (in-range (sub1 n))))
    (set! out (subst oldsym newsym ast)))
  out)

(launch-benchmarks iterate-subst "substitution" "treelang-racket")


