#!r6rs

(library

 (cosh header)

 (export header)

 (import (rnrs))

 (define header
   '((import (rnrs)
             (rnrs mutable-pairs)
             (scheme-tools object-id)
             (only (scheme-tools)
                   pair rest sum pretty-print pe sym+num exact->inexact inexact->exact all-combinations)
             (only (scheme-tools math) random-real random-integer randomize-rng)
             (except (scheme-tools srfi-compat :1) any)
             (scheme-tools hash)
             (only (scheme-tools external) void)
             (only (cosh polycommon) make-root-node make-score-ref)
             (cosh continuation)
             (cosh application)
             (cosh abort)
             (cosh))

     (define (thunk->node thunk)
       (make-root-node (sym+num 'app (object->id (list thunk '())))))
     
     (define (symbolic-prob thunk v)
       (make-score-ref (thunk->node thunk) v))

     (define (symbolic-log1minus expr) ;; TODO: improve
       `(log (- 1.0 (exp ,expr))))

     (define (symbolic-kl A B domain)
       `(+ ,@(map (lambda (v) `(if (or (= ,(symbolic-prob A v) -inf.0)
                                  (= ,(symbolic-prob B v) -inf.0))
                              0.0
                              (* (exp ,(symbolic-prob A v))
                                 (- ,(symbolic-prob A v) ,(symbolic-prob B v)))))
                  domain)))

     (define kl-flip/no-apply
       (vector
        (lambda (self k A B domain)
          (make-continuation k
                             (list #t #f)
                             (list `(- ,(symbolic-kl A B domain))
                                   (symbolic-log1minus `(- ,(symbolic-kl A B domain))))))
        'kl-flip))

     (define flip
       (vector
        (lambda (self k . p)
          (make-continuation k
                             (list #t #f)
                             (if (not (null? p))
                                 (list (log (first p)) (log (- 1.0 (first p))))
                                 (list (log .5) (log .5)))))
        'flip))

     (define log-flip
       (vector
        (lambda (self k . p)
          (make-continuation k
                             (list #t #f)
                             (if (not (null? p))
                                 (list (first p) (log (- 1.0 (exp (first p)))))
                                 (list (log .5) (log .5)))))
        'log-flip))

     (define sample-integer
       (vector
        (lambda (self k n)
          (make-continuation k
                             (iota n)
                             (make-list n (- (log n)))))
        'sample-integer))

     (define sample-discrete
       (vector
        (lambda (self k probs)
          (make-continuation k
                             (iota (length probs))
                             (map log probs)))
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
