#!r6rs

(library

 (cosh preamble)

 (export preamble
         with-preamble)

 (import (rnrs))

 (define (with-preamble expr)
  `(begin
     ,@preamble
     (begin
       ,@expr)))

 (define preamble
   '((define sample (lambda (thunk) (thunk)))

     (define true #t)

     (define false #f)
     
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

     (define exact-query
       (lambda (nfqp) (rejection-query nfqp)))

     (define repeat
       (lambda (N proc)
         (if (= N 0) '() (pair (proc) (repeat (- N 1) proc)))))

     (define (mem proc)
       (let ([mt '()])
         (lambda args
           (let ([entry (assoc args mt)])
             (if entry
                 (cdr entry)
                 (let ([val (cosh-apply proc args)])
                   (set! mt (cons (cons args val) mt))
                   val))))))

     (define (force obj)
       (if (pair? obj)
           (if (eq? (first obj) 'delayed)
               (force ((rest obj)))
               obj)
           obj))     

     (define uniform-draw
       (lambda (lst) 
         (if (null? lst)
             '()
             (list-ref lst (sample-integer (length lst))))))

     (define (%sum-repeat proc n s)
       (if (= n 0)
           s
           (%sum-repeat proc
                        (- n 1)
                        (+ s (proc)))))

     (define (sum-repeat proc n)
       (%sum-repeat proc n 0))
     
     (define (nflip p)
       (if (flip p) 1 0))

     (define (binomial p n)
       (sum-repeat (lambda () (nflip p))
                   n))
     
     (define (multinomial vals probs)
       (list-ref vals (sample-discrete probs)))

     (define (map proc . lsts)
       (if (null? (rest lsts)) (single-map proc (first lsts)) (multi-map proc lsts)))

     (define (single-map proc lst) (if (null? lst) '() (pair (proc (first lst)) (map proc (rest lst)))))

     (define (multi-map proc lsts) ;;takes list of lists and proc of that many arguments.
       (if (null? (first lsts)) '() (pair (cosh-apply proc (single-map first lsts))
                                          (multi-map proc (single-map rest lsts)))))

     (define (many-map proc . lsts) (multi-map proc lsts))

     (define (filter proc lst)
       (if (null? lst)
           '()
           (let ([fst (first lst)]
                 [filtered-rst (filter proc (rest lst))])
             (if (proc fst)
                 (pair fst filtered-rst)
                 filtered-rst))))

     ))

 )