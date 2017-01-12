#! /usr/bin/env racket
#lang typed/racket/no-check

(require "../../common/racket/command_line_runner.rkt"
         "countnodes.rkt"
         (only-in "../../grammar_racket.gib" Toplvl))

(define (iterate-countnodes [ast : Toplvl] [n : Int]) : Toplvl
  (define out (countnodes ast))
  (for ([i (in-range (sub1 n))])
    (set! out (countnodes ast)))
  out)
