#!r6rs

(import (graph)
        (rnrs)
        (rnrs mutable-pairs)
        (scheme-tools))

(define (my-and a b)
  (and a b))

(define (my-or a b)
  (or a b))

(define (thunk)
  (begin
    (define flip
      (vector
       (lambda (self k) k)
       'flip-code))
    (define top
      (vector
       (lambda (self v)
         (begin
           (display "result: ")
           (pretty-print v)        
           v))
       'top-code))
    ;; cc code goes here
    %(cc_code)s
    ))

(print-graph thunk)

