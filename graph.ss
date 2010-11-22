#!r6rs

;; assumptions:
;; - terminal values are not vectors
;; - all random primitives are fair flips

;; data types:
;; table: cont -> conns
;; conns: v -> (p . node)
;;       [conn-1 conn-2 ...]
;; node: cont | value
;; cont: v -> cont | value

(library

 (graph)

 (export print-graph)

 (import (rnrs)
         (only (_srfi :1) first second)
         (scheme-tools)
         (transforms)
         (rhash))

 (define-record-type graph
   (fields (mutable root graph:root graph:set-root!)
           (mutable table graph:table graph:set-table!)
           (mutable frontier graph:frontier graph:set-frontier!))
   (protocol
    (lambda (p)
      (lambda () (p 'empty (make-hash-table) '())))))

 (define (graph:pop-frontier! graph)
   (let ([frontier (graph:frontier graph)])
     (if (null? frontier)
         (error graph "tried to pop empty frontier!")
         (let ([next (first frontier)])
           (graph:set-frontier! graph (rest frontier))
           next))))

 (define (graph:push-frontier! graph node)
   (graph:set-frontier! graph (pair node (graph:frontier graph))))

 (define (graph:add-node! graph node)
   (let ([node-exists (hash-table-ref/default (graph:table graph) node #f)])
     (when (not node-exists)
           (hash-table-set! (graph:table graph) node '())
           (graph:push-frontier! graph node))))

 (define conn->value first)

 (define conn->score second)

 (define (graph:connect! graph parent child value score)
   (let* ([conns (hash-table-ref (graph:table graph) parent)]
          [old-conn (assoc value conns)]
          [new-conn (pair value (pair score child))])
     (if (false? old-conn)
         (hash-table-set! (graph:table graph)
                          parent
                          (pair new-conn conns))
         (assert (equal? old-conn new-conn)))))

 (define/curry (graph:add-child! graph node child value score)
   (graph:add-node! graph child)
   (graph:connect! graph node child value score))

 (define cont? vector?)

 ;; (node, value) -> node
 (define (call node value)
   ((vector-ref node 0) node value))

 (define (get-values node)
   (list #t #f))

 (define (get-scores node)
   (list .5 .5))

 (define (node->graph node)
   (let ([graph (make-graph)])
     (graph:add-node! graph node)
     (graph:set-root! graph node)
     graph))

 ;; step: graph -> graph
 (define (step graph)
   (if (null? (graph:frontier graph))
       #f
       (let ([node (graph:pop-frontier! graph)])
         (when (cont? node)
               (let* ([values (get-values node)]
                      [scores (get-scores node)]
                      [nodes (map (lambda (v) (call node v)) values)])
                 (for-each (graph:add-child! graph node) nodes values scores)))
         graph)))

 ;; explode: graph -> graph
 (define (explode graph)
   (let ([new-graph (step graph)])
     (if (false? new-graph)
         graph
         (explode new-graph))))

 ;; init: thunk -> graph
 (define (init thunk)
   (node->graph (thunk)))

 (define (print-graph thunk)
   (map pretty-print
    (finitize
     (hash-table->alist
      (graph:table
       (explode
        (init thunk)))))))

 )