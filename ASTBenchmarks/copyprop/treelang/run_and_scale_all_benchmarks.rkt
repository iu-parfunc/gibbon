#! /usr/bin/env racket
#lang typed/racket

(require "./copyprop.rkt"
         "../../common/racket/command_line_runner.rkt"
         "../../grammar_racket.gib")

(provide iterate-copyprop)

(define (iterate-copyprop [ast : Toplvl] [n : Integer]) : Toplvl
  (define out (copyprop ast))
  (for ([i (in-range (sub1 n))])
    (set! out (copyprop ast)))
  out)


