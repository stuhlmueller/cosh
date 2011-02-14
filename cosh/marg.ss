#!r6rs

;; Given graph, compute marginal probabilities.

(library

 (cosh marg)

 (export marg-graph)

 (import (rnrs)
         (rnrs mutable-pairs)
         (cosh continuation)
         (scheme-tools)
         (scheme-tools linsolve)
         (scheme-tools graph)
         (scheme-tools queue)
         (scheme-tools mem)
         (scheme-tools hash)
         (scheme-tools object-id))

 (define (node->variable-name node)
   (sym+num 'n
            (if (continuation? node)
                (continuation:id node)
                (object->id node))))

 (define (variable-name->node name)
   (id->object (sym+num->num name)))

 (define (marg-graph graph)
  (pe "marginalization using linear solver ...\n")
  (let-values ([(leaves eqn) (graph->eqns graph)])
    (let ([marginal-values (linsolve eqn)])
      (pe "looking up leaf values ...\n")
      (let ([nodename->prior (alist->hash-table marginal-values finitize-equal? finitize-hash)])
        (map (lambda (leaf-name) (pair (variable-name->node leaf-name)
                                  (hash-table-ref/default nodename->prior leaf-name 'unknown)))
             leaves)))))

 (define (graph->eqns graph)
   (define leaves '())
   (values
    leaves
    (map (lambda (node)
           (when (graph:leaf? graph node)
                 (set! leaves (cons (node->variable-name node) leaves)))
           `(= ,(node->variable-name node)
               ,(if (equal? node (graph:root graph))
                    1.0
                    (let ([links (graph:parent-links graph node)])
                      (if (null? links)
                          (error node "no parent-links found!")
                          `(+ ,@(map (lambda (link) `(* ,(link->weight link)
                                                        ,(node->variable-name (link->target link))))
                                     links)))))))
         (map car (graph->alist graph)))))

 )