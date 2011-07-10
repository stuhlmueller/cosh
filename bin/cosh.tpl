#!r6rs

(import (rnrs)
        (scheme-tools)
        (cosh))

(define expr
  '(
%(code)s

))

(define marginal->value car)
(define marginal->log-prob cdr)
(let ([marginals (cosh expr %(cosh-params)s)])
  (for-each (lambda (marginal)
       (pe (marginal->value marginal) ": "
           (exp (marginal->log-prob marginal))
           " (" (marginal->log-prob marginal) ")\n"))
     marginals)
  (when %(verbose)s
    (pe "\nsum: " (exp (apply logsumexp (map marginal->log-prob marginals))) "\n")))