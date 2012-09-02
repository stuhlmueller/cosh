#!r6rs

(library

 (cosh components)

 (export marginalize-components)

 (import (rnrs)
         (cosh visualize)
         (scheme-tools srfi-compat :1)
         (scheme-tools graph)
         (scheme-tools math iterate)
         (scheme-tools math newton)
         (scheme-tools hash)
         (scheme-tools watcher)
         (scheme-tools)
         (cosh global)
         (cosh polymarg)
         (cosh polycommon))

 (define (equation->symbols equation eqn-filter)
   (cond [(null? equation) '()]
         [(contains? '(= + - * / log exp logsumexp if or) equation eq?) '()]
         [(eqn-filter equation) '()]
         [(symbol? equation) (list equation)]
         [(list? equation) (apply append (map (lambda (eqn) (equation->symbols eqn eqn-filter))
                                              equation))]
         [else '()]))

 (define (equations->symbols equations eqn-filter)
   (unique
    (fold (lambda (equation symbols)
            (append (equation->symbols equation eqn-filter) symbols))
          '()
          equations)))

 ;; Filter out duplicate objects
 (define (unique objects)
   (let ([seen? (make-watcher)])
     (filter (lambda (obj) (not (seen? obj))) objects)))

 ;; Return only those bindings that talk about variables that occur in
 ;; the equations.
 (define (relevant-solution-equations equations solutions)
   (let* ([symbols (equations->symbols equations (lambda (x) #f))]
          [equations (filter-map (lambda (symbol)
                                   (let ([binding (hash-table-ref/default solutions symbol #f)])
                                     (if binding
                                         `(= ,symbol ,binding)
                                         #f)))
                                 symbols)])
     (unique equations)))

 (define (component-equations graph component)
   (fold (lambda (node eqns)
           (append (subgraph->equations graph node) eqns))
         '()
         component))

 (define (free-variables eqns)
   (let ([bound-vars (map second eqns)])
     (equations->symbols (map cddr eqns)
                         (lambda (s) (contains? bound-vars s eq?)))))

 (define (free-variable-equations eqns)
   (map (lambda (var-name)
          `(= ,var-name -inf.0))
        (free-variables eqns)))

 (define (hash-table-set!/assert-consistent table key value)
   (let* ([hash-table-miss (gensym)]
          [existing-value (hash-table-ref/default table key hash-table-miss)])
     (if (eq? existing-value hash-table-miss)
         (hash-table-set! table key value)
         (when (not (equal? existing-value value))
               (begin (pe " " key " is bound to " existing-value ", can't set to " value "\n")
                      (error #f "hash-table-set!/assert-unbound: not unbound"))))))

 (define (iterate-with-message equations)
   (let-values ([(solutions final-delta) (iterate/eqns equations 0.0)])
     (if (not (= final-delta 0.0))
         (begin
           (if verbose
               (pen "fixed-point iterator: final delta " final-delta " -- trying newton...")
               (pe "."))
           (newton equations))
         solutions)))

 ;; component: a list of polymap nodes (= root nodes for subproblems)
 ;; solutions: association list of variable names (?) and values
 ;; return value: new solutions
 (define (marginalize-component! graph component solutions)
   (let* ([equations-1 (component-equations graph component)]
          [equations-2 (relevant-solution-equations equations-1 solutions)]
          [equations-3 (free-variable-equations (append equations-1 equations-2))]
          [equations (append equations-1 equations-2 equations-3)]
          [new-solutions (iterate-with-message equations)])
     (for-each (lambda (binding)
                 (hash-table-set!/assert-consistent solutions
                                                    (first binding)
                                                    (rest binding)))
               new-solutions)))

 ;; Components must be in topological order (i.e. if there is a link
 ;; from component A to component B, A must come first).
 (define (marginalize-components graph components)
   (let ([solutions (make-eq-hash-table)])
     (for-each (lambda (component) (marginalize-component! graph component solutions))
               components)
     (lookup-leaf-values graph solutions)))

 )