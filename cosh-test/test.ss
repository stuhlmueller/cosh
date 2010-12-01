#!r6rs

(import (rnrs)
        (scheme-tools)
        (cosh)
        (cosh-test preamble)
        (cosh-test header)
        (cosh-test pragmatics))

(define simple-expr
  '(list (flip) (flip)))

(define rejection-expr
  '(rejection-query
    (define x (flip .3))
    (define y (flip .5))
    ;; (define z (flip .6))
    (list x y)
    (cosh-or x y)))

(define (test expr)
  (pretty-print
   ((marginalize-expr header
                      (with-preamble expr)) 5)))

;; (test simple-expr)

(test rejection-expr)
;; (test pragmatics-expr)