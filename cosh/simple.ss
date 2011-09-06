#!r6rs

;; In this simplified version of the UDP algorithm, we assume that the
;; domains of all expressions are known (all Boolean) in order not to
;; need cps. We further distinguish functions using eq? in order not
;; to need cc.

;; We lift our program to operate on symbolic distributions and to
;; return a list of equations. Applications marginalize over operator
;; and argument distributions. Using memoization, we compute the
;; equations corresponding to any particular application only once.

;; Input language:
;; define | self-eval | primitive | lambda | if | (A B) | begin | letrec

(import (rnrs)
        (cosh dist)
        (cosh desugar)
        (scheme-tools)
        (scheme-tools srfi-compat :1)
        (transforms syntax)
        (transforms utils)
        (transforms common))


;; --------------------------------------------------------------------
;; Header

(define cosh-env
  (environment '(rnrs)
               '(cosh dist)
               '(scheme-tools)
               '(scheme-tools math symbolic)
               '(scheme-tools math iterate)
               '(scheme-tools srfi-compat :1)
               '(scheme-tools hash)))

(define cosh-header
  '(

    ;; Variable names
    
    (define id-table
      (make-finitize-hash-table))

    (define make-id (get-counter))

    (define (cosh-id obj)
      (hash-table-ref id-table
                      obj
                      (lambda ()
                        (let ([id (make-id)])
                          (hash-table-set! id-table obj id)
                          id))))

    (define (variable obj return)
      (sym-append 'a (cosh-id obj) 'v (cosh-id return)))

    ;; Distribution cache
    
    (define dist-table
      (make-finitize-hash-table))    
    
    (define (cache type f)
      (lambda args
        (let ([obj (cons type args)])
          (when (not (hash-table-exists? dist-table obj))
                (hash-table-set! dist-table obj 'seen)
                (hash-table-set! dist-table obj (apply f args)))
          (make-dist (list #t #f)
                     (list (variable obj #t)
                           (variable obj #f))))))

    ;; Computation of explicit probabilities given
    ;; symbolic distribution and equations.
    
    (define (obj->equations obj dist)
      (map (lambda (v p) `(= ,(variable obj v) ,p))
           (dist-vals dist)
           (dist-probs dist)))

    (define (dist-table->equations table)
      (hash-table-fold table
                       (lambda (obj dist equations)
                         (append (obj->equations obj dist) equations))
                       '()))

    (define (dist-update dist bindings)
      (let ([btable (alist->hash-table bindings eq?)])
        (make-dist (dist-vals dist)
                   (map (lambda (v p)
                          (hash-table-ref btable p (lambda () 'missing)))
                        (dist-vals dist)
                        (dist-probs dist)))))

    (define (dist-solve dist equations)
      (let-values ([(solutions delta) (iterate/eqns equations 0.0 'start-value 0.0)])
        (dist-update dist solutions)))
    
    (define (top obj)
      (pretty-print-dist
       (dist-solve (distify obj)
                   (dist-table->equations dist-table))))

    ;; Lifted apply

    (define (list-apply xs)
      (apply (first xs) (rest xs)))
    
    (define dist-apply
      (cache 'app
             (lambda (op . args)
               (if (not (any dist? (pair op args)))
                   (apply op args)
                   (let* ([factors (map distify (pair op args))]
                          [marginal (dist-product factors list-apply)])
                     (dist-collapse marginal))))))
    
    ;; Lifted if

    (define dist-if
      (cache 'if
             (lambda (test delayed-cons delayed-alt)
               (if (not (dist? test))
                   (if test (delayed-cons) (delayed-alt))
                   (let ([cons-dist (distify (delayed-cons))]
                         [alt-dist (distify (delayed-alt))])
                     (dist-mix (list cons-dist alt-dist)
                               (list (dist-prob test #t) (dist-prob test #f))))))))
    
    ;; Random primitives
    
    (define (flip p)
      (make-dist '(#t #f)
                 (list p (s- 1 p))))    

    ))


;; --------------------------------------------------------------------
;; Lift to distributions

(define primitives (make-parameter '()))

(define (primitive? var)
  (memq var (primitives)))

(define (lift e)
  (define handler
    (match-lambda
     [(:predicate primitive?) e]
     [(:predicate self-evaluating?) e]
     [('letrec defns body)
      (let* ([names (map def->name defns)]
             [vals (map def->val defns)])
        `(letrec ,(zip names (map lift vals))
           ,(lift body)))]
     [('lambda args body) `(lambda ,args ,(lift body))]     
     [('define name body) `(define ,name ,(lift body))]     
     [('if test cons alt) `(dist-if ,(lift test) (lambda () ,(lift cons)) (lambda () ,(lift alt)))]
     [('begin . exprs) `(begin ,@(map lift exprs))]
     [(op . args) `(dist-apply ,(lift op) ,@(map lift args))]
     [else (error e "lift: cannot handle expression type")]))
  (handler e))

(define (lift/top e)
  (parameterize ([primitives (get-primitives e)])
                (lift e)))

(define (cosh expr)
  (let ([lifted-expr (lift/top (de-sugar-toplevel expr))])
    (eval (local (begin-wrap `(,@cosh-header
                               (top ,(local lifted-expr)))))
          cosh-env)))


;; --------------------------------------------------------------------
;; Test

(define test-expr
  '(begin
     (define (foo)
       (if (flip .7)
           (not (foo))
           (flip .3)))
     (foo)))

(cosh test-expr)