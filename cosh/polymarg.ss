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

 ;; go through subgraph top to bottom
 ;; make a hash table that stores for each node what the incoming links are
 ;; convert table to equations
 (define (subgraph->equations graph root)
   (let ([seen? (make-watcher)]
         [equation-table (make-finitize-hash-table)])
     (let nodes->equation-table ([nodes (list root)])
       (when (not (null? nodes))
             (let ([node (first nodes)])
               (if (not (seen? node))
                   (let ([child-links (graph:child-links graph node)])
                     (for-each (lambda (child-link)
                                 (hash-table-set! equation-table
                                                  (link->target child-link)
                                                  (pair (make-link (link->label child-link)
                                                                   (link->weight child-link)
                                                                   node)
                                                        (hash-table-ref/default equation-table
                                                                                (link->target child-link)
                                                                                '()))))
                               child-links)
                     (nodes->equation-table (append (map link->target child-links)
                                                    (rest nodes))))
                   (nodes->equation-table (rest nodes))))))
     (hash-table-fold equation-table
                      (lambda (node links eqns)
                        (pair (node->eqn root node links) eqns))
                      (list (node->eqn root root '())))))
 
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
 
 (define (node->eqn root node parent-links)
   `(= ,(node->variable-name root node)
       ,(if (null? parent-links)
            0.0
            `(logsumexp ,@(map (lambda (link)
                                 `(+ ,(link->variable/weight link)
                                     ,(node->variable-name root (link->target link))))
                               parent-links)))))

 (define (lookup-leaf-values graph solutions)
   (let ([leaves (graph:reachable-terminals graph (graph:root graph))])
     (map (lambda (node)
            (let ([var-name (node->variable-name (graph:root graph) node)])
              (pair node (hash-table-ref solutions var-name))))
          leaves)))

 ;; FIXME: convert to return hash table
 (define (polymarg-graph graph)
   (let* ([equations (polygraph->equations graph)]          
          [solutions (iterate/eqns equations 0.0)])
     (lookup-leaf-values graph solutions)))

 )
