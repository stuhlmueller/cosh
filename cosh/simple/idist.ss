#!r6rs

;; Implicit distributions (defined via system of equations)

(library

 (cosh simple idist)

 (export show-idist
         solve-idist
         make-idist
         idist->vars
         idist->defs
         idist->marginal-vals
         idist->marginal-defs)

 (import (rnrs)
         (cosh simple dist)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (scheme-tools debug)
         (scheme-tools hash)
         (scheme-tools watcher)
         (scheme-tools math iterate)
         (scheme-tools graph)
         (scheme-tools graph components)
         (scheme-tools graph utils))

 ;; utils

 (define (unique objects)
   (let ([seen? (make-watcher)])
     (filter (lambda (obj) (not (seen? obj))) objects)))

 (define (hash-table-set!/assert-consistent table key value)
   (let* ([hash-table-miss (gensym)]
          [existing-value (hash-table-ref/default table key hash-table-miss)])
     (if (eq? existing-value hash-table-miss)
         (hash-table-set! table key value)
         (when (not (equal? existing-value value))
               (begin (pe " " key " is bound to " existing-value ", can't set to " value "\n")
                      (error #f "hash-table-set!/assert-unbound: not unbound"))))))

 (define (iterate-with-message eqns)
   (let ([iterator-env (environment '(rnrs) '(scheme-tools math))])
     (let-values ([(solutions final-delta) (iterate/eqns eqns 0.0 'env iterator-env 'start-value 0.0)])
       (when (not (= final-delta 0.0))
             (pe "fixed-point iterator: final delta " final-delta "\n"))
       solutions)))

 ;; terms

 (define (simplify-term term)
   (define handler
     (match-lambda
      [('* 1.0 x) (simplify-term x)]
      [('* x 1.0) (simplify-term x)]
      [(:predicate list?) (map simplify-term term)]
      [else term]))
   (handler term))

 (define (term->vars term)
   (define handler
     (match-lambda
      [(:or 'logsumexp '+ '= '* '/ '()) '()]
      [(:predicate symbol?) (list term)]
      [(:predicate list?) (apply append (map term->vars term))]
      [else '()]))
   (handler term))

 ;; implicit distributions

 (define idist->vars second)

 (define idist->defs third)

 (define idist->marginal-vals fourth)

 (define idist->marginal-defs fifth)

 (define (make-idist vars defs marginal-vals marginal-defs)
   (list 'idist
         vars
         (map simplify-term defs)
         marginal-vals
         (map simplify-term marginal-defs)))

 (define (show-idist idist)
   (pe "\nEquations:\n")
   (for-each (lambda (var def) (pe var " = " def "\n"))
             (idist->vars idist)
             (idist->defs idist))
   (pe "\nMarginal distribution:\n")
   (for-each (lambda (val def) (pe "p(" val ") = " def "\n"))
             (idist->marginal-vals idist)
             (idist->marginal-defs idist)))

 (define (idist-graph idist)
   (let ([graph (make-graph)]
         [var-def-table (make-eq-hash-table)])
     (for-each (lambda (var def)
                 (hash-table-set! var-def-table var def)
                 (let ([node (graph:add/retrieve! graph var)]
                       [used-vars (term->vars def)])
                   (for-each (lambda (v) (graph:add/link! graph var v v 1.0))
                             used-vars)))
               (idist->vars idist)
               (idist->defs idist))
     (when (debug-mode) (display-graph graph))
     (values graph var-def-table)))

 ;; computing idist solutions via components

 (define (relevant-binding-eqns eqns table)
   (let* ([symbols (unique (term->vars eqns))]
          [eqns (filter-map (lambda (symbol)
                              (let ([binding (hash-table-ref/default table symbol #f)])
                                (if binding
                                    `(= ,symbol ,binding)
                                    #f)))
                            symbols)])
     (unique eqns)))

 (define (component-eqns component var-def-table)
   (map (lambda (var) `(= ,var ,(hash-table-ref var-def-table var
                                           (lambda () (begin
                                                   (when (debug-mode) (pe "\nvar " var " NOT FOUND, using p=0\n"))
                                                   0.0)))))
        component))

 (define (solve-component! eqns solution-table)
   (let* ([sol-eqns (relevant-binding-eqns eqns solution-table)]
          [new-solutions (iterate-with-message (append sol-eqns eqns))])
     (for-each (lambda (binding)
                 (hash-table-set!/assert-consistent solution-table
                                                    (first binding)
                                                    (rest binding)))
               new-solutions)))

 (define (solve-idist idist)
   (let-values ([(graph var-def-table) (idist-graph idist)])
     (let ([components (strongly-connected-components graph)]
           [solution-table (make-eq-hash-table)])
       (for-each (lambda (component)
                   (solve-component! (component-eqns component var-def-table)
                                     solution-table))
                 components)
       (entries->dist
        (map (lambda (val marginal-var)
               (assert (symbol? marginal-var))
               (list val (hash-table-ref solution-table marginal-var)))
             (idist->marginal-vals idist)
             (idist->marginal-defs idist))))))

 )