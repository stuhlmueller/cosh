#!r6rs

;; Given cps-cc-thunk, create context graph.

(library

 (cosh graph)

 (export cc-cps-thunk->graph
         node->graph
         init
         add-root
         step
         explode)

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
   (let ([new-root-cont (make-continuation 'root '(init) '(0.0))]
         [old-root-cont (graph:root graph)])
     (graph:add-node! graph new-root-cont)
     (graph:link! graph new-root-cont old-root-cont 'init 0.0)
     (graph:set-root! graph new-root-cont)
     graph))

 (define (step graph queue)
   (let ([node (dequeue! queue)])
     (when (continuation? node)
           (let* ([values (continuation:support node)]
                  [scores (continuation:scores node)]
                  [nodes (map (lambda (v) (call-continuation node v))
                              values)])
             (for-each (lambda (n v s)
                         (if (graph:node-exists? graph n)
                             (graph:link! graph node n v s)
                             (begin
                               (graph:add-child! graph node n v s)
                               (enqueue! queue n))))
                       nodes values scores)))
     graph))

 (define (explode graph graph-size-limit)
   (let ([queue (make-queue (graph:root graph))])
     (let loop ([graph graph])
       (if (or (and graph-size-limit
                    (> (graph-size graph) graph-size-limit))
               (queue-empty? queue))
           graph
           (loop (step graph queue))))))

 (define (cc-cps-thunk->graph thunk graph-size-limit)
   (add-root (explode (init thunk) graph-size-limit))) 

 )