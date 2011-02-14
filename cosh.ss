#!r6rs

(library

 (cosh)

 (export marg-expr
         marg-cc-cps-thunk
         marg-graph
         polymarg-expr
         polymarg-return-thunk
         polymarg-graph
         compmarg-expr
         expr->graph
         expr->cc-cps-thunk
         expr->return-thunk         
         expr->cc-cps-expr
         cc-cps-thunk->graph
         header->reserved-words)

 (import (rnrs)
         (rnrs eval)
         (transforms)
         (cosh marg)         
         (cosh graph)
         (cosh polymarg)
         (cosh polycommon)
         (cosh polygraph)
         (cosh polymap)
         (cosh components)
         (cosh desugar)
         (scheme-tools)
         (scheme-tools graph)
         (scheme-tools graph components)
         (scheme-tools srfi-compat :1))

 (define (header->reserved-words header)
   (let ([defines (filter (lambda (e) (tagged-list? e 'define)) header)])
     (map (lambda (d) (if (list? (second d)) (caadr d) (second d)))
          defines)))
 
 (define (expr->environment expr)
   (let ([imports (find (lambda (e) (tagged-list? e 'import)) expr)])
     (apply environment (rest imports))))

 (define (expr->body expr)
   (filter (lambda (e) (not (tagged-list? e 'import)))
           expr))

 (define (local expr)
   `((lambda () ,expr)))

 (define (evaluate expr)
   (eval (local (begin-wrap (expr->body expr)))
         (expr->environment expr)))


 ;; linear solver

 (define (expr->cc-cps-expr header expr with-returns)
   `(,@header
     (lambda ()
       ,(transform (de-sugar-toplevel expr)
                   (header->reserved-words header)
                   with-returns))))
 
 ;; (header, expr) -> thunk
 (define (expr->cc-cps-thunk header expr)
   (evaluate (expr->cc-cps-expr header expr #f)))

 ;; (header, expr) -> graph
 (define expr->graph
   ($ cc-cps-thunk->graph
      expr->cc-cps-thunk))
 
 ;; thunk -> dist
 (define marg-cc-cps-thunk
   ($ marg-graph
      cc-cps-thunk->graph))

 ;; (header, expr) -> dist
 (define marg-expr
   ($ marg-cc-cps-thunk
      expr->cc-cps-thunk))

 
 ;; polynomial solver

 ;; (header, expr) -> thunk 
 (define (expr->return-thunk header expr)
   (evaluate (expr->cc-cps-expr header expr #t)))
 
 ;; thunk -> dist
 (define polymarg-return-thunk
   ($ polymarg-graph
      return-thunk->polygraph))

 ;; expr -> dist
 (define polymarg-expr
   ($ polymarg-return-thunk
      expr->return-thunk))

 
 ;; component solver

 ;; expr -> dist
 (define (compmarg-expr header expr)
   (let* ([graph (return-thunk->polygraph (expr->return-thunk header expr))]
          [polymap (polygraph->polymap graph)])
     (let ([components (strongly-connected-components polymap)])
       (marginalize-components graph components))))
 
 )