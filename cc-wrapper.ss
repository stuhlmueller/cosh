#!r6rs

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

(pretty-print
 (transform
  '%(expr)s
  '(flip)))