#!r6rs

;; Given cps-cc-thunk, create context graph.

(library

 (cosh graph)

 (export thunk->graph)

 (import (rnrs)
         (cosh continuation)
         (scheme-tools)
         (scheme-tools hash)
         (scheme-tools graph)
         (scheme-tools queue))

 (define (node->graph node)
   (let ([graph (make-graph)])
     (graph:add-node! graph node)
     (graph:set-root! graph node)
     graph))

 (define (init thunk)
   (node->graph (thunk)))

 (define (add-root graph)
   (let ([new-root-cont (make-continuation 'root '(#t) '(1.0))]
         [old-root-cont (graph:root graph)])
     (graph:add-node! graph new-root-cont)
     (graph:link! graph new-root-cont old-root-cont #t 1.0)
     (graph:set-root! graph new-root-cont)
     graph))

 (define (call node value)
   (let ([clos (continuation:closure node)])
     ((vector-ref clos 0) clos value)))

 (define (get-values node)
   (continuation:support node))

 (define (get-scores node)
   (continuation:scores node)) 

 (define (explode graph)
   (define queue (make-queue (graph:root graph)))
   ;; step: graph -> graph
   (define (step graph)
     (if (queue-empty? queue)
         #f
         (let ([node (dequeue! queue)])
           (when (continuation? node)
                 (let* ([values (get-values node)]
                        [scores (get-scores node)]
                        [nodes (map (lambda (v) (call node v)) values)])
                   (for-each (lambda (n v s)
                               (begin
                                 (graph:add-child! graph node n v s)
                                 (enqueue! queue n)))
                             nodes values scores)))
           graph)))
   ;; explode: graph -> graph
   (define (%explode graph)
     ;; (display ".")
     (let ([new-graph (step graph)])
       (if (false? new-graph)
           graph
           (%explode new-graph))))
   (%explode graph))

 (define thunk->graph
   ($ add-root
      explode
      init))

 )