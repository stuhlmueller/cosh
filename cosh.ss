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
         return-thunk->polygraph
         header->reserved-words)

 (import (rnrs)
         (rnrs eval)
         (transforms)
         (cosh dot)
         (cosh marg)
         (cosh global)
         (cosh graph)
         (cosh polymarg)
         (cosh polycommon)
         (cosh polygraph)
         (cosh polymap)
         (cosh components)
         (cosh desugar)
         (cosh header)
         (cosh preamble)
         (scheme-tools)
         (scheme-tools graph)
         (scheme-tools graph utils)
         (scheme-tools graph components)
         (scheme-tools macros)
         (scheme-tools math)
         (scheme-tools srfi-compat :1)
         (transforms syntax)
         (xitomatl keywords))

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

 ;; (thunk, graph-size-limit) -> dist
 (define (marg-cc-cps-thunk cc-cps-thunk graph-size-limit)
   (marg-graph
    (cc-cps-thunk->graph cc-cps-thunk graph-size-limit)))

 ;; (header, expr) -> dist
 (define (marg-expr header expr graph-size-limit)
   (verbose-pe "\nTIME:\n")
   (let* ([cc-cps-thunk (opt-timeit (verbose) (expr->cc-cps-thunk header expr))]
          [graph (opt-timeit (verbose) (cc-cps-thunk->graph cc-cps-thunk graph-size-limit))]
          [original-graph-size (graph-size graph)]
          [marginals (opt-timeit (verbose) (marg-graph graph))])
     (verbose-pe "\nSPACE:\n"
                 "- graph-size: " original-graph-size "\n")
     marginals))


 ;; polynomial solver

 ;; (header, expr) -> thunk
 (define (expr->return-thunk header expr)
   (evaluate (expr->cc-cps-expr header expr #t)))

 ;; (thunk, graph-size-limit) -> dist
 (define polymarg-return-thunk
   ($ polymarg-graph
      return-thunk->polygraph))

 ;; (expr, graph-size-limit) -> dist
 (define (polymarg-expr header expr graph-size-limit)
   (polymarg-return-thunk (expr->return-thunk header expr)
                          graph-size-limit))


 ;; component solver

 (define (get-component-sizes graph components)
   (map (lambda (comp)
          (apply + (map (lambda (root) (length (subgraph->equations graph root)))
                        comp)))
        components))

 ;; expr -> dist
 (define (compmarg-expr header expr graph-size-limit)
   (verbose-pe "\nTIME:\n")
   (let* ([return-thunk (opt-timeit (verbose) (expr->return-thunk header expr))]
          [graph (opt-timeit (verbose) (return-thunk->polygraph return-thunk graph-size-limit))]
          [original-graph-size (graph-size graph)]
          [polymap (opt-timeit (verbose) (polygraph->polymap graph))]
          [components (opt-timeit (verbose) (strongly-connected-components polymap))]
          [marginals (opt-timeit (verbose) (marginalize-components graph components))])
     (let ([component-sizes (get-component-sizes graph components)])
       (verbose-pe "\nSPACE:\n"
                   "- graph-size: " original-graph-size "\n"
                   "- subproblems: " (graph-size polymap) "\n"
                   "- components: " (length components) "\n"
                   "- mean-component: " (exact->inexact (mean component-sizes)) "\n"
                   "- median-component: " (exact->inexact (median < component-sizes))
                   "\n\n")
       (when (verbose)
             (polygraph->file graph))
       marginals)))


 (define (get-solver state-merging subproblems)
   (cond [(and (not state-merging) (not subproblems)) (error 'get-solver "enumeration solver not available")]
         [(not subproblems) marg-expr]
         [else (lambda args (parameterize ([merge-continuations state-merging]) (apply compmarg-expr args)))]))

 (define/kw (cosh expr
                  [limit :default #f]
                  [verbosity :default #f]
                  [state-merging :default #t]
                  [subproblems :default #t])
   (let ([solver (get-solver state-merging subproblems)])
     (parameterize ([verbose verbosity])
                   (solver header
                           (with-preamble expr)
                           limit))))

 )