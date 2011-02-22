#!r6rs

(import (rnrs)
        (cosh)
        (cosh header)
        (cosh preamble)
        (scheme-tools))

(define flip-expr
  '(

    (flip)

    ))

(define simple-expr
  '(

    (define (foo)
      (if (flip)
          (not (foo))
          #t))

    (foo)

    ))

(define simple-rejection-expr
  '(

    (rejection-query
     (define x (flip))
     (define y (flip))
     (list x y)
     (or x y))

    ))

(define forcingfromabove-expr
  '(

    (define (foo z n)
      (rejection-query
       (define x (flip))
       x
       (if (= n 0)
           #t
           (equal? (foo x (- n 1))
                   z))))

    (foo #t 1)

    ))

(map pretty-print
     (compmarg-expr header
                    (with-preamble forcingfromabove-expr)
                    1000))