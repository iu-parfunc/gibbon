#lang racket

(require racket/performance-hint)
(provide (all-defined-out))

(define-inline (f x) (add1 x))

 (f 99)

(struct cursor (buf [offset #:mutable]) #:transparent)

;; Save a snapshot of the current offset within a buffer:
(define-inline (save-cursor c) ;; Is there a generic copy method for this?
  (cursor (cursor-buf c) (cursor-offset c)))

;; Option 1: vector based implementation of buffers
;--------------------------------------------------------------------------------

;; FINISHME - indirections:
; (define indirection-tag (gensym 'indirection))

(define (new-buffer n)
  (cursor (make-vector n) 0))

(define (buffer-size c) (vector-length (cursor-buf c)))

(define-inline (advance-cursor! c slots)
  (set-cursor-offset! c (+ (cursor-offset c) slots)))

(define-inline (read-slot! c)
  (let* ([off (cursor-offset c)])
    (set-cursor-offset! c (add1 off))
    (vector-ref (cursor-buf c) off)))

(define-inline (read-int64! c) (read-slot! c))

;; We could implement redirections by allowing cursor struct values in
;; tag positions.  
(define-inline (read-tag! c)   (read-slot! c))

(define-inline (write-slot! c x)
  (let* ([off (cursor-offset c)])
    (set-cursor-offset! c (add1 off))
    (vector-set! (cursor-buf c) off x)))

;; (cursor, symbol) -> void
(define-inline (write-tag! c x)   (write-slot! c x))
(define-inline (write-int64! c x) (write-slot! c x))

(define tag-slots   1)
(define int64-slots 1)

;; Option 2: bytestring based:
;--------------------------------------------------------------------------------

;--------------------------------------------------------------------------------
