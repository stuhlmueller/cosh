#!r6rs

(import (rnrs)
        (scheme-tools)
        (cosh)
        (cosh-test preamble)
        (cosh-test header)
        (cosh-test pragmatics))

(define (test expr)
  (map pretty-print
       (marginalize-expr header
                         (with-preamble expr))))

(define simple-expr
  '( (list (flip) (flip)) ))

(define rejection-expr
  '( 

    (define (foo n)
      (rejection-query
       (define x (flip .5))
       x
       (if (= n 0)
           x
           (cosh-or x (bar (- n 1))))))

    (define (bar n)
      (rejection-query
       (define y (flip .5))
       y
       (cosh-or y
                (foo n))))

    (foo 3)

    ))

(define delay-expr
  '(

    (let* ([x1 (delay (flip))]
           [x2 (delay (flip))]
           [x3 (delay (flip))]
           [x4 (delay (flip))]
           [x5 (delay (flip))]
           [x6 (delay (flip))]
           [x7 (delay (flip))]           
           [x8 (delay (flip))])
      (force x1))

    ))

(define rejection-delay-expr
  '(

    (rejection-query
     (define x (delay (flip)))
     (define y (flip))
     (define z (flip))
     (list (force x))
     (cosh-or y z))

    ))

;; (test simple-expr)
;; (test rejection-expr)
;; (test delay-expr)
;; (test rejection-delay-expr)
(test pragmatics-expr)