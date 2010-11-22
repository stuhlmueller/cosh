#!r6rs

(import (rnrs)
        (scheme-tools)
        (transforms)
        (rhash))

(define to-explore '())
(define vals '())

(define (flip)
  ((call/cc
    (lambda (k)
      (k (lambda ()
           (begin
             (set! to-explore (cons (lambda (t v) (k (lambda () (begin (set! to-explore t) (set! vals v) #f)))) to-explore))
             #t)))))))

(define (enumerate proc)
  (set! vals (list (proc)))
  (let loop ()
    (if (null? to-explore)
        vals
        (let* ([proc (car to-explore)]
               [val (proc to-explore vals)])
          (set! to-explore (cdr to-explore))
          (set! vals (cons val vals))
          (loop)))))

(define (my-and a b)
  (and a b))

(define (my-or a b)
  (or a b))

(define (recurse n)
  (if (= n 0)
      (flip)
      ((if (= (mod n 2) 0) my-and my-or)
       (recurse (- n 1)) (recurse (- n 1)))))

(pretty-print (enumerate (lambda () (recurse 2))))
