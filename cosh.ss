#!r6rs

(library

 (cosh)

 (export cosh
         marg-expr
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
         (cosh dot)
         (cosh marg)         
         (cosh graph)
         (cosh polymarg)
         (cosh polycommon)
         (cosh polygraph)
         (cosh polygraph simplify)
         (cosh polymap)
         (cosh components)
         (cosh desugar)
         (cosh header)
         (cosh preamble)
         (scheme-tools)
         (scheme-tools graph)
         (scheme-tools graph utils)
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
 
 ;; (thunk, graph-size-limit) -> dist
 (define polymarg-return-thunk
   ($ polymarg-graph
      return-thunk->polygraph))

 ;; (expr, graph-size-limit) -> dist
 (define (polymarg-expr expr graph-size-limit)
   (polymarg-return-thunk (expr->return-thunk expr)
                          graph-size-limit))

 
 ;; component solver

 ;; expr -> dist
 (define (compmarg-expr header expr graph-size-limit)
   (let* ([graph (return-thunk->polygraph (expr->return-thunk header expr) graph-size-limit)]
          [graph (simplify-polygraph! graph)]
          [polymap (polygraph->polymap graph)])
     ;; (display (polygraph->dot graph))
     (let ([components (strongly-connected-components polymap)])
       (marginalize-components graph components))))


 ;; "cosh" refers to the most current solver, using the default header
 ;; and preamble

 (define (cosh expr . limit)
   (compmarg-expr header
                  (with-preamble expr)
                  (if (null? limit)
                      #f
                      (first limit))))
 
 )