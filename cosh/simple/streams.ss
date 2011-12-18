#!r6rs

;; How do recursive streams ever get marked as done?
;; (assume for now that they don't, test improvement over
;; streams without change detection)

(library

 (cosh simple streams)

 (export (rename (cstream stream)
                 (cstream-map stream-map)
                 (cstream-constant stream-constant)
                 (define-cstream define-stream)
                 (cstream->list stream->list)
                 (cstream-car stream-car)
                 (cstream-cdr stream-cdr)
                 (cstream-cons stream-cons)
                 (cstream-from stream-from)
                 (cstream-lambda stream-lambda)
                 (cstream-let stream-let)
                 (cstream-null stream-null)
                 (cstream-null? stream-null?)
                 (cstream? stream?)))

 (import (rnrs)
         (scheme-tools)
         (srfi :41))

 (define-syntax define-cstream
    (syntax-rules ()
      ((define-cstream (name . formal) body0 body1 ...)
       (define-stream (name . formal) body0 body1 ...))))

 (define-syntax cstream-cons
    (syntax-rules ()
      ((cstream-cons obj strm)
       (stream-cons obj strm))))

 (define-syntax cstream-lambda
    (syntax-rules ()
      ((cstream-lambda formals body0 body1 ...)
       (stream-lambda formals body0 body1 ...))))

 (define-syntax cstream-let
   (syntax-rules ()
     ((cstream-let tag ((name val) ...) body1 body2 ...)
      (stream-let tag ((name val) ...) body1 body2 ...))))

 (define-syntax cstream
   (syntax-rules ()
     ((cstream) stream-null)
     ((cstream x y ...) (stream-cons x (stream y ...)))))

 (define cstream->list stream->list)
 (define cstream-car stream-car)
 (define cstream-cdr stream-cdr)  
 (define cstream-constant stream-constant)
 (define cstream-from stream-from)
 (define cstream-map stream-map)
 (define cstream-null stream-null)
 (define cstream-null? stream-null?)
 (define cstream? stream?)

 (define (constant-stream->cstream strm)
   (stream-cons (pair #t (stream-car strm))
                (stream-map (lambda (x) (pair #f x)) (stream-cdr strm))))

 )

