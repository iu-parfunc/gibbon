#lang racket/base

(require racket/performance-hint racket/fixnum (for-syntax racket/base))
(provide (all-defined-out))

;; Safe mode for debugging:
;(require (rename-in racket/fixnum [fx= unsafe-fx=]))
;; Unsafe mode for performance:
 (require racket/unsafe/ops)


;; Option 1: vector based implementation of buffers
;--------------------------------------------------------------------------------
#;
(begin 
    ;; FINISHME - indirections:
  ; (define indirection-tag (gensym 'indirection))

  (define (new-buffer n)
    (values (make-vector n) 0))

  (define-inline (buffer-size buf off) (unsafe-vector-length buf))

  (define-syntax define-tags
    (syntax-rules ()
      [(define-tags (name ...))
       (begin (define name (quote name)) ...)]))

  (define-inline (tag=? a b) (unsafe-fx= a b))
  
  (define-inline (advance-cursor! buff off slots)
     (unsafe-fx+ off slots))

  (define-inline (read-slot! buff off)
    (values (unsafe-fx+ 1 off)
            (unsafe-vector-ref buff off)))

  (define-inline (read-int64! buff off) (read-slot! buff off))

  ;; We could implement redirections by allowing cursor struct values in
  ;; tag positions.  
  (define-inline (read-tag! buf off)   (read-slot! buf off))

  (define-inline (write-slot! buff off x)
    (begin
      (unsafe-vector-set! buff off x)
      (unsafe-fx+ 1 off)))

  ;; (cursor, symbol) -> void
  (define-inline (write-tag! buff off x)   (write-slot! buff off x))
  (define-inline (write-int64! buff off x) (write-slot! buff off x))

  (define tag-slots   1)
  (define int64-slots 1)
)


;; Option 2: bytestring based:
;--------------------------------------------------------------------------------
#;
(begin 
    ;; FINISHME - indirections:
  ; (define indirection-tag (gensym 'indirection))

  (define (new-buffer n)
    (values (make-bytes n) 0))

  (define-inline (buffer-size buf off) (bytes-length buf))

  (define-syntax (define-tags stx)
    (syntax-case stx ()
      [(define-tags (name ...))
       (with-syntax ([(tags ...) (build-list (length (syntax->list #'(name ...)))  values)])
         
         #'(begin (define name (quote tags)) ...))]))

  (define-inline (tag=? a b) (fx= a b))
  
  (define-inline (advance-cursor! buff off slots)
     (fx+ off slots))

  (define-inline (read-slot! buff off)
    (values (fx+ 1 off)
            (bytes-ref buff off)))

  (define-inline (read-int64! buff off)
    (values
     (fx+ int64-slots off)
     (integer-bytes->integer buff #t ; signed
                             #f off (+ off 8))))


  ;; We could implement redirections by allowing cursor struct values in
  ;; tag positions.  
  (define-inline (read-tag! buf off)   (read-slot! buf off))

  (define-inline (write-slot! buff off x)
    (begin
      (bytes-set! buff off x)
      (fx+ 1 off)))

  ;; (cursor, symbol) -> void
  (define-inline (write-tag! buff off x)   (write-slot! buff off x))
  (define-inline (write-int64! buff off x)
    (integer->integer-bytes
     x int64-slots #t ; signed
     #f ; big endian       
     buff off)
    (+ int64-slots off))
  
  (define tag-slots   1)
  (define int64-slots 8)
  )

  
;-------------------------------------------------------------------------------

;; Option 3 fxvector


(begin 
    ;; FINISHME - indirections:
  ; (define indirection-tag (gensym 'indirection))

  (define (new-buffer n)
    (values (make-fxvector n) 0))

  (define-inline (buffer-size buf off) (unsafe-fxvector-length buf))

  (define-syntax (define-tags stx)
    (syntax-case stx ()
      [(define-tags (name ...))
       (with-syntax ([(tags ...) (build-list (length (syntax->list #'(name ...)))  values)])
         
         #'(begin (define name (quote tags)) ...))]))

  (define-inline (tag=? a b) (unsafe-fx= a b))
  
  (define-inline (advance-cursor! buff off slots)
     (unsafe-fx+ off slots))

  (define-inline (read-slot! buff off)
    (values (unsafe-fx+ 1 off)
            (unsafe-fxvector-ref buff off)))

  (define-inline (read-int64! buff off) (read-slot! buff off))

  ;; We could implement redirections by allowing cursor struct values in
  ;; tag positions.  
  (define-inline (read-tag! buf off)   (read-slot! buf off))

  (define-inline (write-slot! buff off x)
    (begin
      (fxvector-set! buff off x)
      (unsafe-fx+ 1 off)))

  ;; (cursor, symbol) -> void
  (define-inline (write-tag! buff off x)   (write-slot! buff off x))
  (define-inline (write-int64! buff off x) (write-slot! buff off x))

  (define tag-slots   1)
  (define int64-slots 1)
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
