#!r6rs

(import (rnrs)
        (scheme-tools)
        (cosh)
        (cosh visualize)
        (cosh preamble)
        (cosh header)
        (cosh-test pragmatics))

(define (test expr)
  (map pretty-print
       (marg-expr header
                  (with-preamble expr))))

(define (show expr interactive)
  (visualize-graph
   (expr->graph header
                (with-preamble expr))
   interactive))

(define (show-live expr wait)
  (visualize-sampling
   (expr->graph header
                (with-preamble expr))
   wait))

(define simple-expr
  '( (list (flip) (flip)) ))

(define rejection-expr
  '( 

    (define (foo z n)
      (rejection-query
       (define x (delay (flip .5)))
       x
       (if (= n 0)
           x
           (equal? 
            (force (bar x (- n 1)))
            (force z)
            ))))

    (define (bar z n)
      (rejection-query
       (define y (delay (flip .5)))
       y
       (equal? 
        (force (foo y n))
        (force z)
        )))

    (force (bar #t 3))

    ))

;; this illustrates the problem of forcing values "from above" (here:
;; z) in a query, and then, after rejection, losing the advantage of
;; laziness (delayed evaluation of values further up).
(define simple-rejection-expr
  '(

    (define (foo z n)
      (rejection-query
       (define x (delay (flip)))
       x
       (if (= n 0)
           #t
           (equal? (force (foo x (- n 1)))
                   (force z)))))

    (force (foo #t 2))
    
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

    (exact-query
     (define x (delay (flip)))
     (define y (flip))
     (define z true)
     (list (force y))
     (cosh-or y z))

    ))

(define marginalize-test
  '( (define foo (marginalize
                      (lambda (arg) (and (flip arg) (flip 0.5)))) )
         (or (foo 0.1) (foo 0.8) (foo 0.1))
         ))

(define marginalize-test-1
  '((define (flipper) (all (repeat 3 flip)))
    (define foo (marginalize
                 (lambda (arg) (flipper))))
    (define foo2 (marginalize
                  (lambda (arg) (flipper))))
    (or (foo 0.1) (foo 0.8) (foo2 0.1))
    ))


(define marginalize-test-2
 '((define (flipper) (all (repeat 10 flip)))
  (define foo (marginalize
                (lambda (arg) (flipper))))
  (define foo2 (marginalize
                (lambda (arg) (flipper))))
  (or (foo 0.1) (foo 0.8) (foo2 0.2))
  ))

;; (show-live simple-rejection-expr 0.0)
;; (show-live pragmatics-expr #f)
;; (test simple-expr)
;; (test rejection-expr)
;; (test delay-expr)
(test marginalize-test-1)
;; (test pragmatics-expr)