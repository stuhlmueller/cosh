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
             (except (_srfi :69) string-ci-hash string-hash)
             (only (ikarus) void)
             (cosh continuation)
             (cosh))

     ;;this takes a proc and returns a new one that does the cosh thing to get the marginal distribution for each args (by making the call graph and solving),
     ;;then returns a synthetic xrp for this marginal distribution.
     ;;this will only work properly for procs with no side effects (ie. can't close over memoized procs)
     ;;TODO: test thoroughly.
     (define marginalize
       (vector
        (lambda (self k cc-cps-proc)
          (let ((dist-cash (make-hash-table)))
            ((vector-ref k 0) k
                              (vector
                                (lambda (self k . args)
                                  (let ((dist (hash-table-ref dist-cash
                                                              args
                                                              (lambda () 
                                                                (let* ((top-k (vector (lambda (self val) val) 'top-k)) ;;FIXME: need unique top k for each marginal?
                                                                       (dist (marginalize-cc-cps-thunk (lambda () (apply (vector-ref cc-cps-proc 0) cc-cps-proc top-k args)))))
                                                                  (display "dist from my marginal with args  ")(display args)(display ":  ")(display dist)(newline)
                                                                  (hash-table-set! dist-cash args dist)
                                                                  dist)))))
                                    (make-continuation k (map car dist) (map cdr dist))) )
                                'marginalized-proc))))
        'marginalizer))
     (define (map proc lst) (if (null? lst) '() (cons (proc (car lst)) (map proc (cdr lst))))) ;;why isn't map available here already?

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

     (define sample-discrete
       (vector
        (lambda (self k probs)
          (make-continuation k
                             (iota (length probs))
                             probs))
        'sample-discrete-code))
          

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
