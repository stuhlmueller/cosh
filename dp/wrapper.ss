#!r6rs

(import (except (rnrs) map)
        (rnrs mutable-pairs)
        (only (scheme-tools) pair rest sum pretty-print)
        (only (church external math-env) random-real random-integer randomize-rng)
        (except (_srfi :1) any)
        (cosh graph))

;; (define flip
;;   (vector
;;    (lambda (self k . p)
;;      ((vector-ref k 0)
;;       k
;;       (< (random-real)
;;          (if (null? p)
;;              .5
;;              (first p)))))
;;    'flip-code))

(define flip
  (vector
   (lambda (self k . p)
     (make-cont k
                (list #t #f)
                (if (not (null? p))
                    (list (first p) (- 1.0 (first p)))
                    (list .5 .5))))
   'flip-code))

(define sample-integer
  (vector
   (lambda (self k n)
     (make-cont k
                (iota n)
                (make-list n (/ 1.0 n))))
   'sample-integer-code))

(define top
  (vector
   (lambda (self v)
     (begin
       (display "result: ")
       (pretty-print v)        
       v))
   'top-code))

(define v-apply
  (vector
   (lambda (self k proc list-of-args)
     (apply (vector-ref proc 0) (append (list proc k) list-of-args)))
   'apply-code))

;; (define flip
;;   (lambda p
;;     (< (random-real)
;;        (if (null? p)
;;            .5
;;            (first p)))))

;; (define sample-integer
;;   (lambda (n)
;;     (random-integer n)))

(randomize-rng)

(define (thunk)
  (begin
    ;; cc code goes here
    %(prog)s
    ))

(print-marginals thunk)
;; (thunk)