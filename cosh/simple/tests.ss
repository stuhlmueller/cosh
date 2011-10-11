#!r6rs

(import (rnrs)
        (scheme-tools)
        (scheme-tools debug)
        (cosh simple dist)
        (cosh simple streams))


;; --------------------------------------------------------------------
;; Test architecture

(define tests '())

(define-record-type test (fields name expr depth dist tolerance))

(define default-tolerance .01)

(define default-depth 500)

(define-syntax define-test
  (syntax-rules ()
    [(_ name expr dist)
     (begin
       (define name (make-test 'name 'expr default-depth dist default-tolerance))
       (set! tests (append tests (list name))))]
    [(_ name expr dist depth tolerance)
     (begin
       (define name (make-test 'name 'expr depth dist tolerance))
       (set! tests (append tests (list name))))]))

(define (within-tolerance? dista distb tolerance)
  (define (val-within-tolerance? val)
    (< (abs (- (dist-prob dista val) (dist-prob distb val)))
       tolerance))
  (and (all val-within-tolerance? (dist-vals dista))
       (all val-within-tolerance? (dist-vals distb))))

(define (exception? obj)
  (and (pair? obj)
       (eq? (car obj) 'exception)))

(define (safe-evaluate test)
  (call/cc
   (lambda (cont)
     (with-exception-handler
      (lambda (con) (cont (pair 'exception con)))
      (lambda () (cosh (test-expr test) 'depth (test-depth test)))))))

(define (run-test test)
  (pe (test-name test) " ")
  (parameterize
   ([debug-mode #f])
   (let ([ret (safe-evaluate test)])
     (cond [(exception? ret)
            (begin (pe "EXCEPTION.\n  " (cdr ret) "\n") false)]
           [(not (< (- 1.0 (dist-mass ret)) (test-tolerance test)))
            (begin (pe "FAILED.\n  total probability: " (dist-mass ret) "\n") false)]
           [(not (within-tolerance? ret (test-dist test) (test-tolerance test)))
            (begin (pe "FAILED.\n  result: " ret "\n  target: " (test-dist test) "\n") false)]
           [else
            (begin (pe "passed.\n") true)]))))

(define (run-tests . t)
  (let ([results (map run-test (if (null? t) tests (car t)))])
    (pe "\npassed " (length (filter (lambda (x) x) results)) " out of " (length results) " tests.\n")))

(define/kw (debug-test test [depth :default 10] [verbose :default #t])
  (parameterize ([debug-mode verbose])
                (cosh (test-expr test) 'depth depth)))


;; --------------------------------------------------------------------
;; Tests

(define-test raw-primitive
  +
  (make-dist (list +) '(1.0)))

(define-test raw-number
  5
  (make-dist '(5) '(1.0)))

(define-test raw-symbol
  'a
  (make-dist '(a) '(1.0)))

(define-test raw-letrec
  (letrec ([x 1]
           [y 2])
    (+ x y))
  (make-dist '(3) '(1.0)))

(define-test raw-if
  (if (flip .8) #t #f)
  (make-dist '(#t #f) '(.8 .2)))

(define-test raw-begin
  (begin (flip .2) (flip .8))
  (make-dist '(#t #f) '(.8 .2)))

(define-test raw-application
  (not (flip .3))
  (make-dist '(#t #f) '(.7 .3)))

(define-test lambda-variables
  ((lambda (x) (list x x)) (flip))
  (make-dist '((#t #t) (#f #f)) '(.5 .5)))

(define-test let-variables
  (let ([x (flip)]) (list x x))
  (make-dist '((#t #t) (#f #f)) '(.5 .5)))

(define-test letrec-variables
  (letrec ([x (flip)]) (list x x))
  (make-dist '((#t #t) (#f #f)) '(.5 .5)))

(define-test begin-define-variables
  (begin
    (define x (flip))
    (list x x))
  (make-dist '((#t #t) (#f #f)) '(.5 .5)))

(define-test no-recursion
  (begin
    (define (foo p)
      (and (flip p) (flip p)))
    (define (bar)
      (if (flip .5)
          (foo .1)
          (foo .2)))
    (bar))
  (make-dist '(#t #f) '(0.025 0.975)))

(define-test simple-tail-recursion
  (begin
    (define (foo)
      (if (flip .1)
          (foo)
          true))
    (foo))
  (make-dist '(#t) '(1.0)))

(define-test stochastic-arg
  (begin
    (define (bar x)
      x)
    (bar (flip .3)))
  (make-dist '(#t #f) '(.3 .7)))

(define-test stochastic-args
  (begin
    (define (bar x)
      (or x (flip .5)))
    (define (foo x)
      (if (flip .3)
          (flip .1)
          (bar x)))
    (foo (flip .3)))
  (make-dist '(#t #f) '(.485 .515)))

(define-test tail-recursion-stochastic-args
  (begin
    (define (foo x)
      (if (flip .5)
          (foo (not x))
          x))
    (foo (flip .5)))
  (make-dist '(#t #f) '(.5 .5)))

(define-test not-true
  (begin
    (define (foo)
      (if (flip .5)
          (not #t)
          #t))
    (foo))
  (make-dist '(#t #f) '(.5 .5)))

(define-test not-true-nested
  (begin
    (define (bar)
      #t)
    (define (foo)
      (if (flip .5)
          (not (bar))
          #t))
    (foo))
  (make-dist '(#t #f) '(.5 .5)))

(define-test simple-nontail-recursion
  (begin
    (define (foo)
      (if (flip .5)
          (not (foo))
          true))
    (foo))
  (make-dist '(#t #f) (list 2/3 1/3)))

(define-test nontail-recursion
  (begin
    (define (foo)
      (if (flip .4)
          (not (foo))
          (flip .3)))
    (foo))
  (make-dist '(#t #f) '(0.4142857142857142 0.5857142857142856)))

(define-test nontail-if-predicate
  (begin
    (define (foo)
      (if (flip .4)
          (not (foo))
          (flip .3)))
    (define (bar)
      (if (foo)
          (not (bar))
          (flip .6)))
    (bar))
  (make-dist '(#t #f) '(0.541414141414141 0.458585858585858)))

(define-test simple-infinite
  (begin
    (define (foo)
      (if (flip .9)
          '(1)
          (cons 1 (foo))))
    (car (foo)))
  (make-dist '(1) '(1.0)))

(define-test nontail-list-recursion
  (begin
    (define (foo)
      (cons (flip .2)
            (if (flip .1)
                (foo)
                '())))
    (car (foo)))
  (make-dist '(#t #f) '(.2 .8))
  200
  0.02)

(define-test if-mixture
  (begin
    (define (make-foo a b)
      (letrec ([foo (lambda ()
                      (if (flip a)
                          (not (foo))
                          (flip b)))])
        foo))
    (define foo-1 (make-foo .2 .3))
    (define foo-2 (make-foo .5 .7))
    (if (flip .3)
        (foo-1)
        (foo-2)))
  (make-dist '(#t #f)
             '(0.506666666666666 0.493333333333333)))

(define-test if-safety
  (if (= 1 1)
      (flip .3)
      (error 'foo "error"))
  (make-dist '(#t #f) '(.3 .7)))

(define-test high-orders
  (begin
    (define (foo)
      (if (flip .5)
          (let ([f (foo)])
            (lambda () (f)))
          (lambda () (flip .3))))
    (define (apply-down obj)
      (if (procedure? obj)
          (apply-down (obj))
          obj))
    (apply-down (foo)))
  (make-dist '(#t #f) '(.3 .7))
  5000
  0.05)

(define-test desugared-rejection
  (begin
    (define (drq nfqp)
      ((let ([val (nfqp)])
         (if (first val)
             (lambda () ((rest val)))
             (lambda () (drq nfqp))))))
    (define (nfqp)
      (let ([x (flip)])
        (pair x (lambda () x))))
    (drq nfqp))
  (make-dist '(#t) '(1.0)))

(define-test desugared-rejection-2
  (begin
    (define drq
      (lambda (nfqp)
        ((lambda (val)
           (if (first val)
               ((rest val))
               (drq nfqp)))
         (nfqp))))
    (drq
     (lambda ()
       (letrec ((x (flip)))
         (pair x (lambda () x))))))
  (make-dist '(#t) '(1.0)))

(define-test trivial-rejection
  (rejection-query
   (define x (flip))
   x
   x)
  (make-dist '(#t) '(1.0)))

(define-test simple-rejection
  (rejection-query
   (define x (flip))
   (define y (flip))
   (list x y)
   (or x y))
  (make-dist '((#t #t) (#t #f) (#f #t))
             (list 1/3 1/3 1/3)))

(define-test nested-applications
  (begin
    (define (foo n)
      (if (= n 0)
          #t
          (foo (- n 1))))
    (foo 3))
  (make-dist '(#t) '(1.0)))

(define-test forcing-from-above-let
  (letrec ((drq (lambda (nfqp)
                  (let ([val (nfqp)])
                     (if (first val)
                         ((rest val))
                         (drq nfqp)))))
           (forcing-from-above
            (lambda (z n)
              (drq
               (lambda ()
                 (let ((x (flip 0.3)))
                   (pair
                    (if (= n 0)
                        #t
                        (equal?
                         (forcing-from-above
                          x (- n 1))
                         z))
                    (lambda () x))))))))
    (forcing-from-above #t 1))
  (make-dist '(#t #f) '(.5 .5))
  5000
  0.05)

(define-test forcing-from-above-original
  (begin
    (define (forcing-from-above z n)
      (rejection-query
       (define x (flip .3))
       x
       (if (= n 0)
           #t
           (equal? (forcing-from-above x (- n 1))
                   z))))
    (forcing-from-above #t 1))
  (make-dist '(#t #f) '(.5 .5))
  5000
  0.05)

(define-test forcing-from-above-simplified-1
  (begin
    (define (forcing-from-above z n)
      (let* ([x (flip .3)]
             [c (if (= n 0)
                    #t
                    (equal? z (forcing-from-above x (- n 1))))])
        (if c
            x
            (forcing-from-above z n))))
    (forcing-from-above #t 1))
  (make-dist '(#t #f) '(.3 .7))
  5000
  0.05)

(define-test forcing-from-above-simplified-2
  (letrec ((drq (lambda (nfqp)
                  (let ([val (nfqp)])
                    (if (first val)
                        ((rest val))
                        (drq nfqp)))))
           (foo (lambda (n)
                  (drq
                   (lambda () ((lambda (x n)
                            (pair (if (= n 0) #t (foo 0))
                                  (lambda () x))) #t n))))))
    (foo 1))
  (make-dist '(#t #f) '(.5 .5))
  5000
  0.05)

(define-test forcing-from-above-simplified-3
  (letrec ((drq (lambda (nfqp)
                  (let ([val (nfqp)])
                    (if (first val)
                        ((rest val))
                        (drq nfqp)))))
           (bar (lambda (x n)
                  (pair (if (= n 0) #t (foo 0))
                        (lambda () x))))
           (foo (lambda (n)
                  (drq
                   (lambda () (bar #t n))))))
    (foo 1))
  (make-dist '(#t) '(1.))
  5000
  0.05)

(define-test bayes-net
  (let* ([A (flip)]
         [B (flip (if A .1 .3))]
         [C (flip (if A .2 .7))]
         [D (flip (if (and B C) .3 (if (or B C) .4 .5)))])
    D)
  (make-dist '(#t #f) '(.435 .565)))

;; if p is > .5, doesn't halt with probability 1; inference doesn't
;; converge to 1.
;; (define-test nonhalting
;;   (begin
;;     (define (foo)
;;       (if (if (flip .8) (foo) #t)
;;           #f
;;           (foo)))
;;     (foo))
;;   (make-dist '(1) '(1.0)))


;; --------------------------------------------------------------------
;; Main

(run-tests)

;; (pe (debug-test forcing-from-above-original 'depth 1000 'verbose #f))