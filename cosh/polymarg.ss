#!r6rs

;; Given polygraph, compute marginal probabilities by generating a
;; system of polynomial equations and calling an equation solver.

(library
 
 (cosh polymarg)

 (export polymarg-graph
         subgraph->equations
         lookup-leaf-values)

 (import (rnrs)
         (cosh polycommon)
         (scheme-tools)
         (scheme-tools object-id)
         (scheme-tools watcher)
         (scheme-tools graph)
         (scheme-tools graph utils)
         (scheme-tools math iterate)
         (scheme-tools srfi-compat :1)
         (scheme-tools srfi-compat :13)
         (scheme-tools hash)) 

 (define (polygraph->equations graph)
   (union
    (map (lambda (root) (subgraph->equations graph root))
         (graph:root-nodes graph))
    finitize-equal?))
 
 (define (subgraph->equations graph root)
   (let ([seen? (make-watcher)])
     (let* ([leaves (graph:reachable-terminals graph root)])
       (let node->equations ([node root])
         (pair
          (node->eqn graph root node)
          (apply append
                 (map (lambda (child) (if (seen? child) '() (node->equations child)))
                      (graph:children graph node))))))))

 (define (node->variable-name root node)
   (sym-append 'g (node:id root) 'n (node:id node)))

 (define (variable-name->node name)
   (let ([s (symbol->string name)])
     (id->object (string->number (string-drop s (+ (string-index s #\n) 1))))))

 (define (link->variable/weight link)
   (let ([weight (link->weight link)])
     (cond [(number? weight) weight]
           [(score-ref? weight)
            (node->variable-name (score-ref->root weight)
                                 (score-ref->terminal-node weight))]
           [else (error weight "unknown link weight type")])))

 (define (node->eqn graph root node)
   (let ([parent-links (graph:parent-links graph node)])
     `(= ,(node->variable-name root node)
         ,(if (null? parent-links)
              1.0
              `(+ ,@(map (lambda (link)
                           `(* ,(link->variable/weight link)
                               ,(node->variable-name root (link->target link))))
                         (filter (lambda (link) (graph:reachable? graph root (link->target link)))
                                 parent-links)))))))

 (define (lookup-leaf-values graph solutions)
   (let ([leaves (graph:reachable-terminals graph (graph:root graph))])
     (map (lambda (node)
            (let ([var-name (node->variable-name (graph:root graph) node)])
              (pair node (cdr (assoc var-name solutions)))))
          leaves)))

 (define (polymarg-graph graph)
   (let* ([equations (polygraph->equations graph)]          
          [solutions (iterate/plain equations 0.0)])
     (lookup-leaf-values graph solutions)))

 )
