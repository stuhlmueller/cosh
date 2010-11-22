#!r6rs

;; assumptions:
;; - terminal values are not vectors
;; - all random primitives are fair flips

;; data types:
;; table: cont -> conns
;; uptable: cont -> conns
;; conns: v -> (p . node)
;;       [conn-1 conn-2 ...]
;; node: cont | value
;; cont: v -> cont | value

;; fixme:
;; - don't deal with cont objs directly; map to ids
;; - log space

(library

 (graph)

 (export print-graph
         print-marginals)

 (import (rnrs)
         (only (_srfi :1) first second)
         (scheme-tools)
         (scheme-tools queue)
         (transforms)
         (rhash)
         (mem))

 (define-record-type graph
   (fields (mutable root graph:root graph:set-root!)
           (mutable table graph:table graph:set-table!)
           (mutable uptable graph:uptable graph:set-uptable!)
           (mutable frontier graph:frontier graph:set-frontier!))
   (protocol
    (lambda (p)
      (lambda () (p 'empty (make-hash-table) (make-hash-table) '())))))

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
           (begin
             (hash-table-set! (graph:table graph) node '())
             (hash-table-set! (graph:uptable graph) node '()))
           (graph:push-frontier! graph node))))

 (define (graph:children graph node)
   (let ([conns (hash-table-ref/default (graph:table graph) node '())])
     (map conn->node conns)))

 (define (graph:parent-conns graph node)
   (hash-table-ref/default (graph:uptable graph) node '()))

 (define conn->value first)

 (define conn->score second)

 (define conn->node cddr)

 ;; find old connection using value
 (define (table:add-child! table parent child value score)
   (let* ([conns (hash-table-ref table parent)]
          [old-conn (assoc value conns)]
          [new-conn (pair value (pair score child))])
     (if (false? old-conn)
         (hash-table-set! table
                          parent
                          (pair new-conn conns))
         (begin
           ;; (when (not (requal? old-conn new-conn))
           ;;       (display "add-child!")
           ;;       (pretty-print (finitize old-conn))
           ;;       (pretty-print (finitize new-conn))
           ;;       (pretty-print (map equal? (finitize old-conn) (finitize new-conn))))
           (assert (requal? old-conn new-conn))))))

 ;; find old connection using source/target
 (define (table:add-parent! table parent child value score)
   (let* ([conns (hash-table-ref table child)]
          [old-conn (find (lambda (conn) (and (requal? (conn->node conn) parent)
                                         (equal? (conn->value conn) value))) conns)]
          [new-conn (pair value (pair score parent))])
     (if (false? old-conn)
         (hash-table-set! table
                          child
                          (pair new-conn conns))
         (begin
           ;; (when (not (requal? old-conn new-conn))
           ;;       (display "add-parent!")
           ;;       (pretty-print (finitize old-conn))
           ;;       (pretty-print (finitize new-conn))
           ;;       (pretty-print (map equal? (finitize old-conn) (finitize new-conn))))
           (assert (requal? old-conn new-conn))))))

 ;; child - v, p -> parent | value not unique, probabilities don't sum to 1
 ;; parent - v, p -> child | value unique, probabilities sum to 1
 (define (graph:connect! graph parent child value score)
   (table:add-child! (graph:table graph) parent child value score)
   (table:add-parent! (graph:uptable graph) parent child value score))

 (define/curry (graph:add-child! graph node child value score)
   (graph:add-node! graph child)
   (graph:connect! graph node child value score))

 (define (cont? node) (vector? node))

 (define (value? node) (not (vector? node)))

 (define leaf? value?)

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

 ;; nodes are uniquely identified by their value
 (define (marginalize graph)
   (assert (null? (graph:frontier graph)))
   (let ([root (graph:root graph)])
     (define score
       (mem
        (lambda (node)
          (let ([conns (graph:parent-conns graph node)])
            (if (null? conns)
                1.0
                (sum (map (lambda (conn)
                            (* (conn->score conn)
                               (score (conn->node conn))))
                          conns)))))))
     (define scores '())
     (define frontier
       (let ([q (make-empty-queue)])
         (enqueue! q root)
         q))
     (define (store! val p)
       (set! scores (pair (pair val p) scores)))
     (let loop ()
       (if (queue-empty? frontier)
           scores
           (let ([node (dequeue! frontier)])
             (if (leaf? node)
                 (when (not (assoc node scores))
                       (store! node (score node)))
                 (for-each (enqueue-if-new! requal? frontier)
                           (graph:children graph node)))
             (loop))))))

 (define (print-graph thunk)
   (map pretty-print
    (finitize
     (hash-table->alist
      (graph:table
       (explode
        (init thunk)))))))

 (define (print-marginals thunk)
   (map pretty-print
        (marginalize
         (explode
          (init thunk)))))

 )