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

(define header
  '(
    
    ;; Lifted apply

    (define (list-apply xs)
      (apply (first xs) (rest xs)))
    
    (define (dist-apply op . args)
      (if (not (any dist? (pair op args)))
          (apply op args)
          (let* ([factors (map distify (pair op args))]
                 [marginal (dist-product factors list-apply)])
            (dist-collapse marginal))))
    
    ;; Lifted if

    (define (dist-if test delayed-cons delayed-alt)
      (if (not (dist? test))
          (if test (delayed-cons) (delayed-alt))
          (let ([cons-dist (distify (delayed-cons))]
                [alt-dist (distify (delayed-alt))])
            (dist-mix (list cons-dist alt-dist)
                      (list (dist-prob test #t) (dist-prob test #f))))))
    
    ;; Random primitives
    
    (define (flip p)
      (make-dist '(#t #f) (list p (s- 1 p))))

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
  (let ([transformed-expr (append header
                                  (list (lift/top (de-sugar-toplevel expr))))])
    ;; (pretty-print transformed-expr)
    (eval `((lambda () ,(begin-wrap transformed-expr)))
          (environment '(rnrs)
                       '(cosh dist)
                       '(scheme-tools)
                       '(scheme-tools math symbolic)
                       '(scheme-tools srfi-compat :1)
                       '(scheme-tools hash)))))


;; --------------------------------------------------------------------
;; Test

(define test-expr
  '(begin
     (define x (flip 'p1))
     (define y (not (flip .3)))
     (or x y)))

(pretty-print-dist (expr->equations test-expr))