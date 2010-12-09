#!r6rs

(library

 (cosh)

 (export marginalize-expr
         marginalize-cc-cps-thunk
         marginalize-graph
         expr->graph
         expr->cc-cps-thunk
         cc-cps-thunk->graph)

 (import (rnrs)
         (rnrs eval)
         (_srfi :1)
         (transforms)
         (cosh marginalize)
         (cosh graph)
         (cosh desugar)
         (scheme-tools))

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

 ;; (header, expr) -> thunk
 (define/curry (expr->cc-cps-thunk header expr)
   (evaluate
   `(,@header
     (lambda ()
       ,(transform (de-sugar-toplevel expr)
                   (header->reserved-words header))))))

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