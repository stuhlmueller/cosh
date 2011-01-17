#!r6rs

(library

 (cosh header)

 (export header)

 (import (rnrs))

 (define header
   '((import (rnrs)
             (rnrs mutable-pairs)
             (cosh-test utils)
             (scheme-tools object-id)
             (only (scheme-tools) pair rest sum pretty-print pe sym+num)
             (only (scheme-tools math) random-real random-integer randomize-rng)
             (except (scheme-tools srfi-compat :1) any)
             (scheme-tools hash)
             (only (scheme-tools external) void)
             (cosh continuation)
             (cosh application)
             (cosh))

     ;; Marginalizes cc-cps-proc with given args, stores resulting
     ;; distribution in cache table, and returns the distribution.
     (define (marginalize&cache-dist cache cc-cps-proc args)
       (let* ([top-k (vector (lambda (self val) val) 'top-k)]
              [dist (marginalize-cc-cps-thunk (lambda () (apply (vector-ref cc-cps-proc 0) cc-cps-proc top-k args)))])
         (pe "dist from my marginal with args  " args ":  " dist "\n")
         (hash-table-set! cache args dist)
         dist))

     ;; This takes a proc and returns a new one that does the cosh
     ;; thing to get the marginal distribution for each args (by making
     ;; the call graph and solving), then returns a synthetic xrp for
     ;; this marginal distribution.
     ;; This will only work properly for procs with no side effects
     ;; (ie. can't close over memoized procs)
     (define marginalize
       (vector
        (lambda (self k cc-cps-proc)
          (let ([dist-cache (make-equal-hash-table)]
                [proc-id (object->id cc-cps-proc)])
            ((vector-ref k 0) k
             (vector
              (lambda (self k1 . args)
                (let* ([dist-cacher (lambda () (marginalize&cache-dist dist-cache cc-cps-proc args))]
                       [dist (hash-table-ref dist-cache args dist-cacher)])
                  (make-continuation k1 (map car dist) (map cdr dist))) )
              (sym+num 'marginalized-proc proc-id)))))
        'marginalizer))

     (define flip
       (vector
        (lambda (self k . p)
          (make-continuation k
                             (list #t #f)
                             (if (not (null? p))
                                 (list (first p) (- 1.0 (first p)))
                                 (list .5 .5))))
        'flip))

     (define sample-integer
       (vector
        (lambda (self k n)
          (make-continuation k
                             (iota n)
                             (make-list n (/ 1.0 n))))
        'sample-integer))

     (define sample-discrete
       (vector
        (lambda (self k probs)
          (make-continuation k
                             (iota (length probs))
                             probs))
        'sample-discrete))
          

     (define top
       (vector
        (lambda (self top-value)
          (begin
            (display "result: ")
            (pretty-print top-value)        
            top-value))
        'top))

     (define cosh-apply
       (vector
        (lambda (self k proc list-of-args)
          (apply (vector-ref proc 0) (append (list proc k) list-of-args)))
        'apply))

     (randomize-rng)))
   
   )
