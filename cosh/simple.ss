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

;; TODO:
;; - implement lapply
;; - implement cache that stores
;;   - names for return values of each application (as soon as
;;     application seen)
;;   - equations that compute values for these names (as soon as
;;     equations generated)

(import (rnrs)
        (cosh desugar)
        (scheme-tools)
        (scheme-tools srfi-compat :1)
        (except (transforms syntax) begin-wrap)
        (transforms utils)
        (transforms common))


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
     [('if test cons alt) `(if ,(lift test) ,(lift cons) ,(lift alt))]
     [('begin . exprs) `(begin ,@(map lift exprs))]
     [(op . args) `(lapply ,(lift op) ,@(map lift args))]
     [else (error e "lift: cannot handle expression type")]))
  (handler e))

(define (lift/top e)
  (parameterize ([primitives (get-primitives e)])
                (lift e)))


;; --------------------------------------------------------------------
;; Test

(define simple-header
  '((define-record-type dist (fields vals probs))
    (define (lapply op . args)
      (apply op args))
    (define (flip p)
      (make-dist '(#t #f) (list p (- 1 p))))))

(define (expr->equations expr)
  (let ([transformed-expr (append simple-header
                                  (list (lift/top (de-sugar-toplevel expr))))])
    (pretty-print transformed-expr)
    (eval `((lambda () ,(begin-wrap transformed-expr)))
          (environment '(rnrs)))))

(define test-expr
  '(begin
     (define x (flip .5))
     (define y (flip .5))
     (or x y)))

(pretty-print (expr->equations test-expr))