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
                '(cosh simple idist)
                '(scheme-tools)
                '(scheme-tools math symbolic)
                '(scheme-tools math iterate)
                '(scheme-tools srfi-compat :1)
                '(scheme-tools hash)
                '(scheme-tools debug)
                '(srfi :41)))

 (define (make-cosh-header depth)
   `(

    ;; Variable names

    (define id-table
      (make-finitize-hash-table))

    (define app-counter (get-counter))

    (define return-counter (get-counter))

    (define (cosh-id obj counter)
      (hash-table-ref id-table
                      obj
                      (lambda ()
                        (let ([id (counter)])
                          (hash-table-set! id-table obj id)
                          id))))

    (define (variable app return)
      (sym-append 'a (cosh-id app app-counter)
                  'v (cosh-id return return-counter)))

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

     ;; Distribution streams

     (define (dist:add-variable-info dist app)
       (make-dist (dist-vals dist)
                  (dist-probs dist)
                  (map (lambda (v) (variable app v)) (dist-vals dist))))

     (define-stream (stream:add-variable-info dist-strm app)
       (stream-map (lambda (dist) (dist:add-variable-info dist app))
                   dist-strm))

     ;; Stream merging

     (define (make-diagonal-policy)
       (let ([level 1]
             [step 0])
         (define-stream (this-diagonal-policy strms)
           (when (debug-mode) (pe "diagonal-policy " step "/" level "\n"))
           (if (= step level)
               (begin
                 (set! level (+ level 1))
                 (set! step 0)
                 (stream-cons
                  (stream-car (stream-car strms))
                  (stream-merge this-diagonal-policy
                                (stream-cdr strms))))
               (begin
                 (set! step (+ step 1))
                 (stream-cons
                  (stream-car (stream-car strms))
                  (stream-merge this-diagonal-policy
                                (stream-cons (stream-cdr (stream-car strms))
                                             (stream-cdr strms)))))))
         this-diagonal-policy))

     (define (stream-merge policy strms)
       (define-stream (stream-merge strms)
         (cond ((stream-null? strms) stream-null)
               ((not (stream? (stream-car strms)))
                (error 'stream-merge "non-stream object in input stream"))
               ((stream-null? (stream-car strms))
                (stream-merge (stream-cdr strms)))
               (else (policy strms))))
       (if (not (stream? strms))
           (error 'stream-merge "non-stream argument")
           (stream-merge strms)))

     ;; Stream fast forward

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
       (stream-lambda
        objs
        (when (debug-mode) (pe "\nlooking up " objs " in cache, "))
        (let ([strm (hash-table-ref/default stream-table objs #f)])
          (if strm
              (begin
                (when (debug-mode) (pe "found.\n  car: " (stream-car strm) ". "))
                (let ([ff-stream (stream-fast-forward strm)])
                  (hash-table-set! stream-table objs ff-stream)
                  (when (debug-mode) (pe "\n  ff-ed. car: " (stream-car ff-stream) ". "))
                  (let ([vi-stream (stream:add-variable-info ff-stream objs)])
                    (when (debug-mode) (pe "\n  vi-ed. car: " (stream-car vi-stream) ".\n"))
                    vi-stream)))
              (begin
                (when (debug-mode) (pe "not found.\n"))
                (let ([strm (stream-cons default (streamify (apply f objs)))])
                  (hash-table-set! stream-table objs strm)
                  (let* ([%strm-cdr (streamify (apply f objs))]
                         [next (stream-car %strm-cdr)]
                         [strm-cdr (stream-cons next (stream-cdr %strm-cdr))])
                    (stream:add-variable-info strm-cdr objs))))))))

     (define (stream-table->idist stream-table strm)
       (let ([vars '()]
             [defs '()]
             [marginal-vals '()]
             [marginal-defs '()])
         (hash-table-walk
          stream-table
          (lambda (app strm)
            (let ([dist (stream-car (stream-fast-forward strm))])
              (for-each (lambda (v p)
                          (begin
                            (set! vars (cons (variable app v) vars))
                            (set! defs (cons p defs))))
                        (dist-vals dist)
                        (dist-probs dist)))))
         (let ([top-dist (stream-car (stream-fast-forward strm))])
           (for-each (lambda (v p)
                       (let ([var (variable 'top v)])
                         (begin
                           (set! vars (cons var vars))
                           (set! defs (cons p defs))
                           (set! marginal-vals (cons v marginal-vals))
                           (set! marginal-defs (cons var marginal-defs)))))
                     (dist-vals top-dist)
                     (dist-prob-defs top-dist)))
         (make-idist vars
                     defs
                     marginal-vals
                     marginal-defs)))

     ;; Distribution stream apply

     (define mem-list-apply-to-stream
       (stream-mem (stream-lambda (xs) (streamify (apply (first xs) (rest xs))))
                   null-dist))

     (define-stream (dist-apply-to-stream . dists)
       (let* ([combos (all-combinations (map dist->entries dists))]
              [streams (map (lambda (combo) (mem-list-apply-to-stream (map entry->val combo)))
                            combos)]
              [probs (map (lambda (combo) (apply s* (map entry->prob combo))) combos)])
         (if (null? streams)
             (stream null-dist)
             (let ([mixed-stream (stream-map* (lambda dists (dist-mix dists probs))
                                              streams)])
               mixed-stream))))

     (define-stream (stream-apply . maybe-strms)
       (let* ([streams (map streamify maybe-strms)]
              [merged-stream (stream-merge (make-diagonal-policy)
                                           (stream-map* dist-apply-to-stream streams))])
         merged-stream))

     ;; Distribution stream if

     (define (dist-if test-dist cons-dist alt-dist)
       (when (debug-mode) (pe "dist-if: " test-dist " ? " cons-dist " : " alt-dist "\n"))
       (dist-mix (list cons-dist alt-dist)
                 (list (dist-prob test-dist #t)
                       (dist-prob test-dist #f))))

     (define (delayed? obj)
       (and (pair? obj)
            (eq? (car obj) 'delayed)
            (procedure? (cdr obj))))

     (define (force obj)
       (if (delayed? obj)
           ((cdr obj))
           obj))

     (define (merge-if-streams test-strm cons alt)
       (define-stream (merge-if test-strm cons alt)
         (when (debug-mode) (pe "merge-if\n"))
         (if (stream-null? test-strm)
             stream-null
             (let ([test-dist (stream-car test-strm)])
               (cond [(and (equal? (dist-prob test-dist #t) 0.0)
                           (equal? (dist-prob test-dist #f) 0.0))
                      (stream-cons null-dist
                                   (merge-if (stream-cdr test-strm)
                                             cons
                                             alt))]
                     [(equal? (dist-prob test-dist #t) 0.0)
                      (let ([alt-stream (streamify (force alt))])
                        (stream-cons (stream-car alt-stream)
                                     (merge-if (stream-cdr test-strm)
                                               cons
                                               (stream-cdr alt-stream))))]
                     [(equal? (dist-prob test-dist #f) 0.0)
                      (let ([cons-stream (streamify (force cons))])
                        (stream-cons (stream-car cons-stream)
                                     (merge-if (stream-cdr test-strm)
                                               (stream-cdr cons-stream)
                                               alt)))]
                     [else
                      (let ([cons-stream (streamify (force cons))]
                            [alt-stream (streamify (force alt))])
                        (stream-cons (dist-if test-dist
                                              (stream-car cons-stream)
                                              (stream-car alt-stream))
                                     (merge-if (stream-cdr test-strm)
                                               (stream-cdr cons-stream)
                                               (stream-cdr alt-stream))))]))))
       (if (not (stream? test-strm))
           (error test-strm "merge-if-streams: test is not a stream")
           (if (any (lambda (v) (not (delayed? v))) (list cons alt))
               (error (list cons alt) "merge-if-streams: cons/alt are not delayed")
               (merge-if test-strm cons alt))))

     (define-stream (stream-if test delayed-cons delayed-alt)
       (if (not (stream? test))
           (streamify (if test (force delayed-cons) (force delayed-alt)))
           (merge-if-streams test delayed-cons delayed-alt)))

     ;; Random primitives

     (define-stream (flip . args)
       (let ([p (if (null? args) .5 (car args))])
         (stream-constant
          (make-dist (list #t #f)
                     (list p (s- 1 p))))))

     (define-stream (geometric . args)
       (let ([p (if (null? args) .5 (car args))])
         (stream-map (lambda (n)
                       (make-dist (iota n)
                                  (map (lambda (i) (expt p (+ i 1))) (iota n))))
                     (stream-from 0))))

     ;; Top-level

     (define (show-stream-cache-size)
       (pe "\nstream cache size: " (hash-table-size stream-table) "\n"))

     (define (top maybe-strm)
       (parameterize [(dist-hash-table-maker make-finitize-hash-table)]
                     (let* ([strm (streamify maybe-strm)]
                            [lst (stream->list ,depth strm)]
                            [idist (stream-table->idist stream-table strm)]
                            [solution-dist (solve-idist idist)])
                       (when (debug-mode)
                             (pe "\nfirst non-null dist after: "
                                 (list-index (lambda (dist) (not (null? (dist-vals dist)))) lst))
                             (show-stream-cache-size)
                             (show-idist idist)
                             (pe "\nSolutions:\n")
                             (pretty-print-dist solution-dist))
                       solution-dist)))

     ))


 ;; --------------------------------------------------------------------
 ;; Preamble

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
                  (cons
                   (cdr bindings)
                   (if (lambda-binding? binding)
                       (if within-letrec
                           (list #f #t
                                 (lambda (more-bindings body)
                                   (generator (cons binding more-bindings) body)))
                           (list #f #t
                                 (lambda (more-bindings body)
                                   (generator '() `(%letrec (,binding ,@more-bindings) ,body)))))
                       (if within-let
                           (list #t #f
                                 (lambda (more-bindings body)
                                   (generator (cons binding more-bindings) body)))
                           (list #t #f
                                 (lambda (more-bindings body)
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
      [('if test cons alt) `(stream-if ,(lift test)
                                       (cons 'delayed (lambda () ,(lift cons)))
                                       (cons 'delayed (lambda () ,(lift alt))))]
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
