#!r6rs

;; In this simplified version of the UDP algorithm, we associate each
;; application with a stream of submeasures. We don't assume known
;; domains. We distinguish functions using eq? in order not to need
;; cc. We directly stream (partial) probability distributions; we do
;; not first build equations and solve by fixed-point iteration.

;; Input language:
;; define | self-eval | primitive | lambda | if | (A B) | begin | letrec

(library

 (cosh simple streams)

 (export cosh
         debug-mode)

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

     ;; Stream cache

     (define stream-table
       (make-finitize-hash-table))

     (define (stream-mem f default)
       (lambda objs
         (when (debug-mode) (pe "\nlooking up " objs " in cache, "))
         (if (hash-table-exists? stream-table objs)
             (begin
               (when (debug-mode) (pe "found.\n"))
               (hash-table-ref stream-table objs))
             (begin
               (when (debug-mode) (pe "not found.\n"))
               (let ([strm (streamify (apply f objs))])
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
                     (list p (s- 1 p))))))

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

     (define (merge-if-streams test-strm cons alt)
       (define-stream (merge-if test-strm cons alt)
         (if (stream-null? test-strm)
             stream-null
             (let ([test-dist (stream-car test-strm)])
               (cond [(and (= (dist-prob test-dist #t) 0.0)
                           (= (dist-prob test-dist #f) 0.0))
                      (stream-cons null-dist
                                   (merge-if (stream-cdr test-strm)
                                             cons
                                             alt))]
                     [(= (dist-prob test-dist #t) 0.0)
                      (stream-cons (stream-car (streamify (alt)))
                                   (merge-if (stream-cdr test-strm)
                                             cons
                                             (lambda () (stream-cdr (streamify (alt))))))]
                     [(= (dist-prob test-dist #f) 0.0)
                      (stream-cons (stream-car (streamify (cons)))
                                   (merge-if (stream-cdr test-strm)
                                             (lambda () (stream-cdr (streamify (cons))))
                                             alt))]
                     [else
                      (stream-cons (dist-if test-dist (stream-car (streamify (cons))) (stream-car (streamify (alt))))
                                   (merge-if (stream-cdr test-strm)
                                             (lambda () (stream-cdr (streamify (cons))))
                                             (lambda () (stream-cdr (streamify (alt))))))]))))
       (if (not (stream? test-strm))
           (error test-strm "merge-if-streams: test is not a stream")
           (if (any (lambda (v) (not (procedure? v))) (list cons alt))
               (error (list cons alt) "merge-if-streams: cons/alt are not delayed")
               (merge-if test-strm cons alt))))

     (define (stream-if test delayed-cons delayed-alt)
       (if (not (stream? test))
           (streamify (if test (delayed-cons) (delayed-alt)))
           (merge-if-streams test delayed-cons delayed-alt)))

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

 )
