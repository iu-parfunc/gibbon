#lang racket

(require racket/performance-hint)
(require (for-syntax (only-in racket range)))
(provide (all-defined-out))

;; Safe mode for debugging:
(require (rename-in racket/fixnum [fx= unsafe-fx=]))
;; Unsafe mode for performance:
; (require racket/unsafe/ops)

;;--------------------------------------------------------------------------------


(define-inline (f x) (add1 x))

(f 99)

(struct cursor (buf [offset #:mutable]) #:transparent)

;; Save a snapshot of the current offset within a buffer:
(define-inline (save-cursor c) ;; Is there a generic copy method for this?
  (cursor (cursor-buf c) (cursor-offset c)))

;; Option 1: vector based implementation of buffers
;--------------------------------------------------------------------------------

#;
(begin 
  ;; FINISHME - indirections:
  ; (define indirection-tag (gensym 'indirection))

  (define (new-buffer n)
    (cursor (make-vector n) 0))

  (define (buffer-size c) (vector-length (cursor-buf c)))

  (define-syntax define-tags
    (syntax-rules ()
      [(define-tags (name ...))
       (begin (define name (quote name)) ...)]))

  (define-inline (tag=? a b) (eq? a b))
  
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
)

;; Option 2: bytestring based:
;--------------------------------------------------------------------------------

(begin 

  (define (new-buffer n) (cursor (make-bytes n) 0))

  (define (buffer-size c) (bytes-length (cursor-buf c)))

  (define-syntax define-tags
    (lambda (x)
      (syntax-case x ()
        [(define-tags (name ...))
         (let ([lst (syntax->list #'(name ...))])
           #`(begin
               #,@(datum->syntax x
                   (for/list ([nm lst]
                              [ind (in-range 255 (- 255 (length lst)) -1)])
                     #`(define #,nm #,(datum->syntax nm ind)))))
           )])))

  (define-inline (tag=? a b) (unsafe-fx= a b))

  (define-inline (advance-cursor! c bytes)
    (set-cursor-offset! c (+ (cursor-offset c) bytes)))

  (define-inline (read-tag! c)
    (let* ([off (cursor-offset c)])
      (set-cursor-offset! c (add1 off))
      (bytes-ref (cursor-buf c) off)))

  (define-inline (read-int64! c)
    (let* ([off (cursor-offset c)])
      (set-cursor-offset! c (+ int64-slots off))
      (integer-bytes->integer (cursor-buf c) #t ; signed
                              #f off (+ off 8))))

  (define-inline (write-tag! c x)
    (let* ([off (cursor-offset c)])
      (set-cursor-offset! c (add1 off))
      (bytes-set! (cursor-buf c) off x)))

  (define-inline (write-int64! c x)
    (let* ([off (cursor-offset c)])
      (set-cursor-offset! c (+ int64-slots off))
      (integer->integer-bytes
       x 8 #t ; signed
       #f ; big endian       
       (cursor-buf c) off)))
  
  (define tag-slots   1) ; single byte
  (define int64-slots 8) ; (ctype-sizeof _llong)
  )
  
;--------------------------------------------------------------------------------

;; Example:
#;
(begin 
  (define-tags (Leaf Node))
  (let ([b (new-buffer 30)])
    (write-tag! b Node)
    (write-tag! b Leaf) (write-int64! b 34)
    (write-tag! b Leaf) (write-int64! b 12)
    (bytes->list (cursor-buf b))))

