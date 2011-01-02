#!r6rs

(library

 (cosh-test header)

 (export header)

 (import (rnrs))

 (define header
   '((import (except (rnrs) map)
             (rnrs mutable-pairs)
             (cosh-test utils)
             (only (scheme-tools) pair rest sum pretty-print)
             (only (church external math-env) random-real random-integer randomize-rng)
             (except (_srfi :1) any)
             (cosh continuation))

     (define flip
       (vector
        (lambda (self k . p)
          (make-continuation k
                             (list #t #f)
                             (if (not (null? p))
                                 (list (first p) (- 1.0 (first p)))
                                 (list .5 .5))))
        'flip-code))

     (define sample-integer
       (vector
        (lambda (self k n)
          (make-continuation k
                             (iota n)
                             (make-list n (/ 1.0 n))))
        'sample-integer-code))

     (define top
       (vector
        (lambda (self top-value)
          (begin
            (display "result: ")
            (pretty-print top-value)        
            top-value))
        'top-code))

     (define cosh-apply
       (vector
        (lambda (self k proc list-of-args)
          (apply (vector-ref proc 0) (append (list proc k) list-of-args)))
        'apply-code))

     (randomize-rng)))
   
   )
