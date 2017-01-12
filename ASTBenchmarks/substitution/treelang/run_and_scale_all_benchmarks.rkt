#! /usr/bin/env racket
#lang typed/racket

(require "./subst_gibbon.gib"
         "../../common/racket/command_line_runner.rkt"
         "../../grammar_racket.gib")

(define oldsym 'call-with-values) ;; Hardcode this, doesn't matter.
(define newsym (string->symbol (string-append (symbol->string oldsym) "99")))

(define (iterate-subst [ast : Toplvl] [n : Integer]) : Toplvl
  (define out (subst oldsym newsym ast))
  (for ((i (in-range (sub1 n))))
    (set! out (subst oldsym newsym ast)))
  out)

(launch-benchmarks iterate-subst "substitution" "treelang-racket")


