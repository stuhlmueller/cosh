#!r6rs

(import (rnrs)
        (scheme-tools)
        (cosh)
        (cosh-test preamble)
        (cosh-test header)
        (cosh-test pragmatics)
        (cosh continuation))

(define rejection-expr
  '(rejection-query
    (define x (flip .3))
    (define y (flip .5))
    ;; (define z (flip .6))
    (list x y)
    (cosh-or x y)))

(define thunk (expr->cc-cps-thunk header (with-preamble rejection-expr)))
(define c (thunk))
(define c-f (call-continuation c #f))
(define c-f-t (call-continuation c-f #t))

(define c-t (call-continuation c #t))
(define c-t-t (call-continuation c-t #t))
(define c-f-f (call-continuation c-f #f))
(define c-t-f (call-continuation c-t #f))

(pe "c:     " c "\n"
    "c-t:   " c-t "\n"
    "c-f:   " c-f "\n"
    "c-t-t: " c-t-t "\n"
    "c-t-f: " c-t-f "\n"
    "c-f-t: " c-f-t "\n"
    "c-f-f: " c-f-f "\n")