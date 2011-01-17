#!r6rs

(library

 (cosh)

 (export marginalize-expr
         marginalize-cc-cps-thunk
         marginalize-graph
         expr->graph
         expr->cc-cps-thunk
         expr->return-thunk         
         expr->cc-cps-expr
         cc-cps-thunk->graph
         header->reserved-words)

 (import (rnrs)
         (rnrs eval)
         (transforms)
         (cosh marginalize)
         (cosh graph)
         (cosh desugar)
         (scheme-tools)
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

 (define (expr->cc-cps-expr header expr with-returns)
   `(,@header
     (lambda ()
       ,(transform (de-sugar-toplevel expr)
                   (header->reserved-words header)
                   with-returns))))

 ;; (header, expr) -> thunk
 (define (expr->cc-cps-thunk header expr)
   (evaluate (expr->cc-cps-expr header expr #f)))

 ;; (header, expr) -> thunk 
 (define (expr->return-thunk header expr)
   (evaluate (expr->cc-cps-expr header expr #t)))

 ;; (header, expr) -> graph
 (define expr->graph
   ($ cc-cps-thunk->graph
      expr->cc-cps-thunk))

 ;; thunk -> dist
 (define marginalize-cc-cps-thunk
   ($ marginalize-graph
      cc-cps-thunk->graph)) 

 ;; (header, expr) -> dist
 (define marginalize-expr
   ($ marginalize-cc-cps-thunk
      expr->cc-cps-thunk))

 )