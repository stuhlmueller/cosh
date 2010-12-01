#!r6rs

(import (cosh graph)
        (rnrs)
        (scheme-tools))

(define (test)
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
    (define (thunk)
      ((vector-ref flip 0)
       flip
       (vector (lambda (self x) ((vector-ref flip 0)
                            flip
                            (vector (lambda (self y) ((vector-ref top 0) top (list (vector-ref self 2) y)))
                                    'l2-code
                                    x)))
               'l1-code)))
    (print-marginals thunk)))

(test)