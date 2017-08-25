#lang racket

(require "common.gib")
(require "compiler.gib")

(define gensym
  (let ([counter 0])
    (lambda ([x 'g])
      (if (integer? x)
          (set! counter x)
          (begin0 (string->unreadable-symbol
                   (format "~a~a" x counter))
            (set! counter (add1 counter)))))))

(define (remove f l)
  (filter (compose not f) l))

(define (true? x)
  (eqv? x #t))

(define (assert-eq-X86 name prg expected)
  (let* ([actual (compile prg)])
    (gensym 0)
    (if (equal? actual expected)
        #t
        `(,name ,expected "not equal to" ,actual))))

(define t1
  (ProgramR (LetER (vector 'a (LitER 2))
                   (AddER (LitER 40) (VarER 'a)))))

(define expected-t1
  (ProgramX86 (ConsSym 'a0 (ConsSym 'tmp1 (NullSyms))) 16 (ConsInstr (MOVQ (LitEX86 2) (DerefE 'RBP -8)) (ConsInstr (MOVQ (LitEX86 40) (DerefE 'RBP -16)) (ConsInstr (MOVQ (DerefE 'RBP -8) (RegE 'RAX)) (ConsInstr (ADDQ (RegE 'RAX) (DerefE 'RBP -16)) (ConsInstr (MOVQ (DerefE 'RBP -16) (RegE 'RAX)) (NullInstrs))))))))

(define tests
  (list (assert-eq-X86 't1 t1 expected-t1)))

;; Not running this now, because the equal? check above doesn't work
#;
(let ([failures (remove true? tests)])
  (if (empty? failures)
      (println "All tests passed.")
      (begin (printf "~a tests failed\n\n" (length failures))
             (map println failures)
             (exit 1))))
