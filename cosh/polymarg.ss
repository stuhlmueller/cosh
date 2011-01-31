#!r6rs

;; Given polygraph, compute marginal probabilities by generating a
;; system of polynomial equations and calling an equation solver.

(library
 
 (cosh polymarg)

 (export polymarg-graph)

 (import (rnrs)
         (cosh polycommon)
         (scheme-tools)
         (scheme-tools object-id)
         (scheme-tools watcher)
         (scheme-tools graph)
         (scheme-tools graph utils)
         (scheme-tools polysolve)
         (scheme-tools srfi-compat :1)
         (scheme-tools srfi-compat :13)
         (scheme-tools hash)) 
 
 (define (graph:root-nodes graph)
   (filter (lambda (node) (null? (graph:parents graph node)))
           (map first (graph->alist graph))))

 (define (polygraph->eqns graph)
   (union
    (map (lambda (root) (root->eqns graph root))
         (graph:root-nodes graph))
    finitize-equal?))
 
 (define (root->eqns graph root)
   (let ([seen? (make-watcher)])
     (let* ([leaves (graph:reachable-terminals graph root)]
            [constraint-eqn `(= (+ ,@(map (lambda (leaf) (node->variable-name root leaf)) leaves)) 1.0)])
       (pair constraint-eqn
             (let node->eqns ([node root])
               (pair
                (node->eqn graph root node)
                (apply append
                       (map (lambda (child) (if (seen? child) '() (node->eqns child)))
                            (graph:children graph node)))))))))

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

 (define (polymarg-graph graph)
   (let* ([eqns (polygraph->eqns graph)]
          [leaves (graph:reachable-terminals graph (graph:root graph))]
          [solutions (polysolve eqns)])
     (map (lambda (solution)
            (map (lambda (node)
                   (let ([var-name (node->variable-name (graph:root graph) node)])
                     (pair node (cdr (assoc var-name solution)))))
                 leaves))
          solutions)))

 )
