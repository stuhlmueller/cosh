#!r6rs

(library

 (cosh-test preamble)

 (export preamble)

 (import (rnrs))

 (define preamble
   '((define sample (lambda (thunk) (thunk)))

     (define (cosh-or a b)
       (if a
           #t
           (if b
               #t
               #f)))

     (define (cosh-and a b)
       (if a
           (if b
               #t
               #f)
           #f))

     (define all
       (lambda (lst)
         (if (null? lst)
             #t
             (if (first lst)
                 (all (rest lst))
                 #f))))

     (define any
       (lambda (lst)
         (if (null? lst)
             #f
             (if (first lst)
                 #t
                 (any (rest lst))))))

     (define rejection-query
       (lambda (nfqp)
         (let ((val (nfqp)))
           (if (first val)
               ((rest val))
               (rejection-query nfqp)))))

     (define repeat
       (lambda (N proc)
         (if (= N 0) '() (pair (proc) (repeat (- N 1) proc)))))

     (define uniform-draw
       (lambda (lst) 
         (if (null? lst)
             '()
             (list-ref lst (sample-integer (length lst))))))

     (define (map proc . lsts)
       (if (null? (rest lsts)) (single-map proc (first lsts)) (multi-map proc lsts)))

     (define (single-map proc lst) (if (null? lst) '() (pair (proc (first lst)) (map proc (rest lst)))))

     (define (multi-map proc lsts) ;;takes list of lists and proc of that many arguments.
       (if (null? (first lsts)) '() (pair (cosh-apply proc (single-map first lsts))
                                          (multi-map proc (single-map rest lsts)))))

     (define (many-map proc . lsts) (multi-map proc lsts))))

 )