#!r6rs

;; Continuation hash

;; This illustrates how 

(import (rnrs)
        (scheme-tools)
        (transforms))

(define (transform expr reserved-words)
  (cc-transform
   (cps-transform
    (assignment-transform
     (letrec-to-set
      expr))
    reserved-words)
   reserved-words))

(define (wrap e)
  `(begin
     (define flip
       (vector (lambda (self k) (pretty-print k))))
    ,@(cdr e)))

(define te
  (transform '(begin (+ 1 (if (flip) 2 3)))
             '(flip)))

(pretty-print (wrap te))