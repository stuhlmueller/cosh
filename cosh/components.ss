#!r6rs

(library

 (cosh components)

 (export marginalize-components)

 (import (rnrs)
         (cosh visualize)
         (scheme-tools srfi-compat :1)
         (scheme-tools graph)
         (scheme-tools math iterate)
         (scheme-tools hash)
         (scheme-tools watcher)
         (scheme-tools)
         (cosh polymarg)
         (cosh polycommon))

 (define (equation->symbols equation)
   (cond [(null? equation) '()]
         [(symbol? equation) (list equation)]
         [(list? equation) (apply append (map equation->symbols equation))]
         [else '()]))
 
 (define (equations->symbol-index equations)
   (let ([table (make-eq-hash-table)])
     (map (lambda (equation)
            (map (lambda (symbol) (hash-table-set! table symbol #t))
                 (equation->symbols equation)))
          equations)
     table))

 ;; Return only those bindings that talk about variables that occur in
 ;; the equations.
 (define (relevant-bindings equations bindings)
   (let* ([symbol-table (equations->symbol-index equations)]
          [relevant? (lambda (binding) (hash-table-ref/default symbol-table (first binding) #f))])
     (filter relevant? bindings)))

 ;; Filter out duplicate bindings.
 (define (unique-bindings bindings)
   (let ([seen? (make-watcher)])
     (filter (lambda (binding) (not (seen? binding))) bindings)))
 
 ;; Takes in an association list of variable names and values,
 ;; generates equations.
 (define (bindings->equations bindings)
   (map (lambda (binding) `(= ,(first binding) ,(rest binding)))
        bindings))

 (define (component-equations graph component)
   (union (map (lambda (root) (subgraph->equations graph root))
               component)
          equal?))

 ;; component: a list of polymap nodes (= root nodes for subproblems)
 ;; solutions: association list of variable names (?) and values
 ;; return value: new solutions
 (define (marginalize-component graph component solutions)
   (let* ([equations-1 (component-equations graph component)]
          [equations-2 (bindings->equations
                        (unique-bindings
                         (relevant-bindings equations-1
                                            solutions)))]
          [equations (append equations-2 equations-1)])
     (iterate equations 0.0)))

 ;; Components must be in topological order (i.e. if there is a link
 ;; from component A to component B, A must come first).
 (define (marginalize-components graph components)
   (let loop ([components components]
              [solutions '()])
     (if (null? components)
         (lookup-leaf-values graph solutions)
         (let ([component-solutions (marginalize-component graph (first components) solutions)])
           (loop (rest components)
                 (append solutions component-solutions))))))

 )