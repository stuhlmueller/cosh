#!r6rs

;; In this simplified version of the UDP algorithm, we associate each
;; application with a stream of submeasures. We don't assume known
;; domains. We distinguish functions using eq? in order not to need
;; cc. We directly stream (partial) probability distributions; we do
;; not first build equations and solve by fixed-point iteration.

;; Input language:
;; define | self-eval | primitive | lambda | if | (A B) | begin | letrec

(import (rnrs)
        (cosh simple dist)
        (cosh desugar)
        (scheme-tools)
        (scheme-tools debug)
        (scheme-tools srfi-compat :1)
        (transforms syntax)
        (transforms utils)
        (transforms common))

(define cosh-env
  (environment '(rnrs)
               '(cosh simple dist)
               '(scheme-tools)
               '(scheme-tools math symbolic)
               '(scheme-tools math iterate)
               '(scheme-tools srfi-compat :1)
               '(scheme-tools hash)
               '(scheme-tools debug)
               '(srfi :41)))

(define (make-cosh-header depth)
  `(

    ;; Streams

    (define (streamify maybe-strm)
      (if (stream? maybe-strm)
          maybe-strm
          (begin
            (assert (not (dist? maybe-strm)))
            (stream-constant (singleton-dist maybe-strm)))))

    (define (stream-map* proc streams)
      (apply stream-map
             (cons proc streams)))
    
    (define (stream-merge policy strms)
      (define stream-merge
        (stream-lambda
         (strms)
         (cond [(stream-null? strms) stream-null]
               [(not (stream? (stream-car strms)))
                (error 'stream-merge "non-stream object in input stream")]
               [(stream-null? (stream-car strms))
                (stream-merge (stream-cdr strms))]
               [else (policy strms)])))
      (if (not (stream? strms))
          (error 'stream-merge "non-stream argument")
          (stream-merge strms)))

    (define (stream-eager? strm)
      (assert (stream? strm))
      (let ([accessor (record-accessor (record-rtd strm) 0)])
        (eq? (car (accessor strm)) 'eager)))

    (define (stream-lazy? strm)
      (assert (stream? strm))
      (let ([accessor (record-accessor (record-rtd strm) 0)])
        (eq? (car (accessor strm)) 'lazy)))

    (define (stream-access strm accessor-index)
      (assert (stream-eager? strm))
      (let* ([box-accessor (record-accessor (record-rtd strm) 0)]
             [strm-pare (cdr (box-accessor strm))]
             [obj-accessor (record-accessor (record-rtd strm-pare) accessor-index)]
             [obj (obj-accessor strm-pare)])
        obj))

    (define (stream-kar strm)
      (stream-access strm 0))

    (define (stream-kdr strm)
      (let ([strm (stream-access strm 1)])
        (assert (stream? strm))
        strm))

    (define (stream-fast-forward strm)
      (define-stream (fast-forward strm prev)
        (if (and (stream-eager? strm) (stream-eager? (stream-kar strm)))
            (fast-forward (stream-kdr strm) strm)
            prev))
      (fast-forward strm strm))

    ;; Stream cache

    (define stream-table
      (make-finitize-hash-table))

    (define (stream-mem f default)
      (lambda objs
        (when (debug-mode) (pe "\nlooking up " objs " in cache, "))
        (if (hash-table-exists? stream-table objs)
            (begin
              (when (debug-mode) (pe "found.\n"))
              (let ([strm (stream-fast-forward (hash-table-ref stream-table objs))])
                (hash-table-set! stream-table objs strm)
                strm))
            (begin
              (when (debug-mode) (pe "not found.\n"))
              (let ([strm (stream-cons default
                                       (streamify (apply f objs)))])
                (hash-table-set! stream-table objs strm)
                strm)))))

    (define (show-stream-cache)
      (pe "\nstream cache:\n")
      (hash-table-walk stream-table
                       (lambda (key strm)
                         (pe key ": " (stream->list 5 strm) "... \n"))))

    (define (show-stream-cache-size)
      (pe "\nstream cache size: " (hash-table-size stream-table) "\n"))

    ;; Random primitives

    (define (flip . args)
      (let ([p (if (null? args) .5 (car args))])
        (stream-constant
         (make-dist (list #t #f)
                    (list p (- 1 p))))))

    (define (geometric . args)
      (let ([p (if (null? args) .5 (car args))])
        (stream-map (lambda (n)
                      (make-dist (iota n)
                                 (map (lambda (i) (expt p (+ i 1))) (iota n))))
                    (stream-from 0))))

    ;; Distribution stream apply & if

    (define (list-apply-to-stream xs)
      (for-each (lambda (v) (assert (not (stream? v)))) xs)
      (streamify (apply (first xs) (rest xs))))

    (define memoized-list-apply-to-stream
      (stream-mem list-apply-to-stream null-dist))

    (define (dist-apply-to-stream . dists)
      (assert (all dist? dists))
      (let* ([combos (all-combinations (map dist->entries dists))]
             [streams (map (lambda (combo) (memoized-list-apply-to-stream (map entry->val combo))) combos)]
             [probs (map (lambda (combo) (apply s* (map entry->prob combo))) combos)])
        (if (null? streams)
            (stream null-dist)
            (stream-map* (lambda dists (dist-mix dists probs))
                         streams))))    

    (define (stream-apply . maybe-strms)
      (let* ([streams (map streamify maybe-strms)])
        (stream-concat (stream-map* dist-apply-to-stream streams))))
    
    (define (dist-if test-dist cons-dist alt-dist)
      (dist-mix (list cons-dist alt-dist)
                (list (dist-prob test-dist #t)
                      (dist-prob test-dist #f))))

    (define (merge-if-streams test-strm cons-strm alt-strm)
      (define-stream (merge-if test-strm cons-strm alt-strm)
        (if (exists stream-null? (list test-strm cons-strm alt-strm))
            stream-null
            (let ([test-dist (stream-car test-strm)])
              (cond [(and (= (dist-prob test-dist #t) 0.0)
                          (= (dist-prob test-dist #f) 0.0))
                     (stream-cons null-dist
                                  (merge-if (stream-cdr test-strm)
                                            cons-strm
                                            alt-strm))]
                    [(= (dist-prob test-dist #t) 0.0)
                     (stream-cons (stream-car alt-strm)
                                  (merge-if (stream-cdr test-strm)
                                            cons-strm
                                            (stream-cdr alt-strm)))]
                    [(= (dist-prob test-dist #f) 0.0)
                     (stream-cons (stream-car cons-strm)
                                  (merge-if (stream-cdr test-strm)
                                            (stream-cdr cons-strm)
                                            alt-strm))]
                    [else
                     (stream-cons (dist-if test-dist (stream-car cons-strm) (stream-car alt-strm))
                                  (merge-if (stream-cdr test-strm)
                                            (stream-cdr cons-strm)
                                            (stream-cdr alt-strm)))]))))
      (if (any (lambda (strm) (not (stream? strm)))
               (list test-strm cons-strm alt-strm))
          (error 'merge-if-streams "non-stream argument")
          (merge-if test-strm cons-strm alt-strm)))

    (define (stream-if test delayed-cons delayed-alt)
      (if (not (stream? test))
          (streamify (if test (delayed-cons) (delayed-alt)))
          (let ([test-strm test]
                [cons-strm (streamify (delayed-cons))]
                [alt-strm (streamify (delayed-alt))])
            (merge-if-streams test-strm cons-strm alt-strm))))

    ;; Top-level

    (define (top maybe-strm)
      (parameterize [(dist-hash-table-maker make-finitize-hash-table)]
                    (let* ([strm (streamify maybe-strm)]
                           [lst (stream->list ,depth strm)])
                      (when (debug-mode)
                            (pe "\n\n")
                            (for-each ppe lst)
                            (pe "\nfirst non-null dist after: "
                                (list-index (lambda (dist) (not (null? (dist-vals dist))))
                                            lst))
                                (show-stream-cache-size))
                      (stream-ref strm ,depth))))

    ))


(define cosh-preamble
  '(

    (define rejection-query
      (lambda (nfqp)
        (let ((val (nfqp)))
          (if (first val)
              ((rest val))
              (rejection-query nfqp)))))

    ))

(define (with-preamble expr)
  `(begin
     ,@cosh-preamble
     ,expr))


;; --------------------------------------------------------------------
;; Lift to distributions

(define primitives (make-parameter '()))

(define (primitive? var)
  (memq var (primitives)))

(define (lambda-binding? binding)
  (and (pair? (second binding))
       (eq? (first (second binding)) 'lambda)))

(define (split-letrec bindings body)
  (define (split-bindings bindings within-let within-letrec generator)
    (if (null? bindings)
        (generator '() body)
        (let ([binding (car bindings)])
          (apply split-bindings
                 (cons (cdr bindings)
                       (if (lambda-binding? binding)
                           (if within-letrec
                               (list #f #t (lambda (more-bindings body)
                                             (generator (cons binding more-bindings) body)))
                               (list #f #t (lambda (more-bindings body)
                                             (generator '() `(%letrec (,binding ,@more-bindings) ,body)))))
                           (if within-let
                               (list #t #f (lambda (more-bindings body)
                                             (generator (cons binding more-bindings) body)))
                               (list #t #f (lambda (more-bindings body)
                                             (generator '() `(let* (,binding ,@more-bindings) ,body)))))))))))
  (de-sugar-all (split-bindings bindings #f #f (lambda (b x) x))))

(define (lift e)
  (define handler
    (match-lambda
     [(:predicate primitive?) e]
     [(:predicate self-evaluating?) e]
     [('letrec defns body) (lift (split-letrec defns body))]
     [('%letrec defns body)
      (let* ([names (map def->name defns)]
             [vals (map def->val defns)])
        `(letrec ,(zip names (map lift vals))
           ,(lift body)))]
     [('lambda args body) `(lambda ,args ,(lift body))]
     [('define name body) `(define ,name ,(lift body))]
     [('if test cons alt) `(stream-if ,(lift test) (lambda () ,(lift cons)) (lambda () ,(lift alt)))]
     [('begin . exprs) `(begin ,@(map lift exprs))]
     [(op . args) `(stream-apply ,(lift op) ,@(map lift args))]
     [else (error e "lift: cannot handle expression type")]))
  (handler e))

(define (lift/top e)
  (parameterize ([primitives (get-primitives e)])
                (lift e)))

(define/kw (cosh expr [depth :default 10])
  (let ([lifted-expr (lift/top (de-sugar-toplevel (with-preamble expr)))])
    (when (debug-mode) (pretty-print lifted-expr))
    (eval (local (begin-wrap `(,@(make-cosh-header depth)
                               (top ,(local lifted-expr)))))
          cosh-env)))


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

(define (run-tests)
  (let ([results (map run-test tests)])
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

(define-test nontail-list-recursion
  (begin
    (define (foo)
      (cons (flip .2)
            (if (flip .1)
                (foo)
                '())))
    (car (foo)))
  (make-dist '(#t #f) '(.2 .8))
  50
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
  3000
  0.03)

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
                  ((let ([val (nfqp)])
                     (if (first val)
                         (lambda () ((rest val)))
                         (lambda () (drq nfqp)))))))
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
    (forcing-from-above #t 4))
  (make-dist '(#t #f) '(.5 .5))
  3000
  0.05)

(define-test forcing-from-above
  (begin
    (define (forcing-from-above z n)
      (rejection-query
       (define x (flip .3))
       x
       (if (= n 0)
           #t
           (equal? (forcing-from-above x (- n 1))
                   z))))
    (forcing-from-above #t 10))
  (make-dist '(#t #f) '(.5 .5))
  3000
  0.05)

(define-test bayes-net
  (let* ([A (flip)]
         [B (flip (if A .1 .3))]
         [C (flip (if A .2 .7))]
         [D (flip (if (and B C) .3 (if (or B C) .4 .5)))])
    D)
  (make-dist '(#t #f) '(.435 .565)))

;; --------------------------------------------------------------------
;; Main

(run-tests)

;; (pe (debug-test bayes-net 'depth 30 'verbose #t))