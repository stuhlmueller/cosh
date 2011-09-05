#!r6rs

;; In this simplified version of the UDP algorithm, we assume that the
;; domains of all expressions are known in order not to need cps. We
;; further distinguish functions using eq? in order not to need cc.

;; We lift our program to operate on symbolic distributions, i.e., to
;; return a list of equations. Applications marginalize over operator
;; and argument distributions. Using memoization, we compute the
;; equations corresponding to any particular equation only once.

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
               '(scheme-tools srfi-compat :1)
               '(scheme-tools hash)))

(define cosh-header
  '(

    (define id-table
      (make-finitize-hash-table))

    (define dist-table
      (make-finitize-hash-table))    

    (define make-id (get-counter))
    
    (define (cosh-id obj)
      (hash-table-ref id-table
                      obj
                      (lambda ()
                        (let ([id (make-id)])
                          (hash-table-set! id-table obj id)
                          id))))
    
    (define (variable app val)
      (sym-append 'a (cosh-id app) 'v (cosh-id val)))        
    
    (define (cache type f)
      (lambda args
        (let ([id (cons type args)])
          (when (not (hash-table-exists? dist-table id))
                (hash-table-set! dist-table id 'seen)
                (hash-table-set! dist-table id (apply f args)))
          (make-dist (list #t #f)
                     (list (variable id #t)
                           (variable id #f))))))

    (define (build-equations expr dist)
      (map (lambda (v p) `(= ,(variable expr v) ,p))
           (dist-vals dist)
           (dist-probs dist)))

    (define (dist-table->equations table)
      (hash-table-fold table
                       (lambda (key val acc)
                         (append (build-equations key val) acc))
                       '()))

    (define (top obj)
      (for-each pretty-print (dist-table->equations dist-table))
      (pe "\nMarginal distribution:\n")
      (if (dist? obj)
          (pretty-print-dist obj)
          (pretty-print obj)))
    
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

(define (expr->equations expr)
  (let* ([lifted-expr (lift/top (de-sugar-toplevel expr))]
         [transformed-expr `(,@cosh-header
                             (top ,(local lifted-expr)))])
    (eval (local (begin-wrap transformed-expr))
          cosh-env)))


;; --------------------------------------------------------------------
;; Test

(define test-expr
  '(begin
     (define (foo)
       (if (flip .5)
           (not (foo))
           (flip .3)))
     (foo)))

(expr->equations test-expr)