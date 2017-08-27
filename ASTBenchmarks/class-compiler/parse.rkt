#lang racket

(require "common.gib")

(provide parse)

(define filepath "")

;; copied from p523 utilities.rkt
(define (input-prog path)
  (set! filepath path)
  (call-with-input-file path
    (lambda (f)
      `(program . ,(for/list ([e (in-port read f)]) e)))))

(define (parse* prg)
  (match prg
    [`,x #:when (symbol? x) (VarER x)]
    [`,n #:when (number? n) (LitER n)]
    [`(program ,e) (ProgramR (parse* e))]
    [`(let ([,x ,rhs]) ,bod) (LetER (vector x (parse* rhs))
                                    (parse* bod))]
    [`(+ ,x ,y) (AddER (parse* x) (parse* y))]
    [`(- ,x) (NegER (parse* x))]
    [x (error (format "parse*: unexpected form, ~a" x))]))

(define (parse path)
  (parse* (input-prog path)))
