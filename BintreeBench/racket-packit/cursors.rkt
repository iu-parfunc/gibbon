#lang racket/base

(require racket/performance-hint racket/fixnum (for-syntax racket/base))
(provide (all-defined-out))

;; Safe mode for debugging:
;(require (rename-in racket/fixnum [fx= unsafe-fx=]))
;; Unsafe mode for performance:
(require racket/unsafe/ops)

(define-syntax (define-tags stx)
  (syntax-case stx ()
    [(define-tags (name ...))
     (with-syntax ([(tags ...)
                    (build-list (length (syntax->list #'(name ...))) values)])
       #'(begin (define name (quote tags)) ...))]))

(define-inline (tag=? a b) (unsafe-fx= a b))


;; Option 1: vector based implementation of buffers
;--------------------------------------------------------------------------------
(define-syntax-rule
  (define-vector-ops
    new-buffer buffer-size advance-cursor! read-slot!
    read-int64! read-tag! write-slot! write-tag! write-int64!
    int64-slots tag-slots)
(begin 
    ;; FINISHME - indirections:
  ; (define indirection-tag (gensym 'indirection))

  (define (new-buffer n)
    (values (make-vector n) 0))

  (define-inline (buffer-size buf off) (unsafe-vector-length buf))

  
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
))


;; Option 2: bytestring based:
;--------------------------------------------------------------------------------
(define-syntax-rule
  (define-bytes-ops
    new-buffer buffer-size advance-cursor! read-slot!
    read-int64! read-tag! write-slot! write-tag! write-int64!
    int64-slots tag-slots)
  (begin 
    ;; FINISHME - indirections:
    ; (define indirection-tag (gensym 'indirection))
    
    (define tag-slots   1)
    (define int64-slots 8)
    (define (new-buffer n)
      (values (make-bytes n) 0))

    (define-inline (buffer-size buf off)
      (unsafe-bytes-length buf))
    
    (define-inline (advance-cursor! buff off slots)
      (unsafe-fx+ off slots))
    
    (define-inline (read-slot! buff off)
    (values (unsafe-fx+ 1 off)
            (unsafe-bytes-ref buff off)))

  (define-inline (read-int64! buff off)
    (values
     (unsafe-fx+ int64-slots off)
     (integer-bytes->integer buff #t ; signed
                             #f off (+ off 8))))


  ;; We could implement redirections by allowing cursor struct values in
  ;; tag positions.  
  (define-inline (read-tag! buf off)   (read-slot! buf off))

  (define-inline (write-slot! buff off x)
    (begin
      (unsafe-bytes-set! buff off x)
      (unsafe-fx+ 1 off)))

  ;; (cursor, symbol) -> void
  (define-inline (write-tag! buff off x)   (write-slot! buff off x))
  (define-inline (write-int64! buff off x)
    (integer->integer-bytes
     x int64-slots #t ; signed
     #f ; big endian       
     buff off)
    (+ int64-slots off))
  
  ))

  
;-------------------------------------------------------------------------------

;; Option 3 fxvector

(define-syntax-rule
  (define-fxvector-ops
    new-buffer buffer-size advance-cursor! read-slot!
    read-int64! read-tag! write-slot! write-tag! write-int64!
    int64-slots tag-slots)
(begin 
    ;; FINISHME - indirections:
  ; (define indirection-tag (gensym 'indirection))

  (define (new-buffer n)
    (values (make-fxvector n) 0))

  (define-inline (buffer-size buf off) (unsafe-fxvector-length buf))
  
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
      (unsafe-fxvector-set! buff off x)
      (unsafe-fx+ 1 off)))

  ;; (cursor, symbol) -> void
  (define-inline (write-tag! buff off x)   (write-slot! buff off x))
  (define-inline (write-int64! buff off x) (write-slot! buff off x))

  (define tag-slots   1)
  (define int64-slots 1)
))




;--------------------------------------------------------------------------------
(require (for-syntax racket/syntax))
(define-syntax (define-ops stx)
  (syntax-case stx ()
    [(_ tag)
     (with-syntax ([(names ...)
                    (map (lambda (i) (datum->syntax #'tag i))
                         '(new-buffer buffer-size advance-cursor! read-slot!
                           read-int64! read-tag! write-slot! write-tag!
                           write-int64!
                           int64-slots tag-slots))]
                   [mac (format-id #'tag "define-~a-ops" #'tag)])
       #'(mac names ...))]))
