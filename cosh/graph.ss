#!r6rs

;; data types:
;; table: cont -> conns
;; uptable: cont -> conns
;; conns: v -> (p . node)
;;       [conn-1 conn-2 ...]
;; node: cont | value
;; cont: (closure, support, scores)
;; closure: v -> cont | value

;; fixme:
;; - don't deal with cont objs directly; map to ids
;; - log space

(library

 (graph)

 (export print-graph
         print-marginals
         make-cont)

 (import (rnrs)
         (only (_srfi :1) first second third fourth)
         (scheme-tools)
         (scheme-tools queue)
         (transforms)
         (scheme-tools hash)
         (scheme-tools mem))

 (define-record-type graph
   (fields (mutable root graph:root graph:set-root!)
           (mutable table graph:table graph:set-table!)
           (mutable uptable graph:uptable graph:set-uptable!)
           (mutable frontier graph:frontier graph:set-frontier!))
   (protocol
    (lambda (p)
      (lambda () (p 'empty (make-hash-table) (make-hash-table) '())))))

 (define clos-symbol-maker (symbol-maker 'clos))
 (define clos->symbol-table (make-hash-table))
 (define symbol->clos-table (make-hash-table))

 (define (make-closure-symbol-entries clos sym)
   (hash-table-set! clos->symbol-table
                    clos
                    sym)
   (hash-table-set! symbol->clos-table
                    sym
                    clos))

 (define (closure->symbol clos)
   (hash-table-ref clos->symbol-table
                   clos
                   (lambda ()
                     (let ([sym (clos-symbol-maker)])
                       (make-closure-symbol-entries clos sym)
                       sym))))

 (define (symbol->closure sym)
   (hash-table-ref symbol->clos-table
                   sym
                   (lambda () (error sym "no closure found!"))))

 ;; this is a list in order to make it easy for hashing to deconstruct it
 (define (make-cont closure support scores)
   (list 'cont (closure->symbol closure) support scores))
 (define cont:closure second)
 (define cont:support third)
 (define cont:scores fourth)
 (define (cont? obj)
   (tagged-list? obj 'cont))

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
           ;; (display "*")
           (hash-table-set! (graph:table graph) node '())
           (hash-table-set! (graph:uptable graph) node '())
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

 (define (value? node) (not (cont? node)))

 (define leaf? value?)

 ;; (node, value) -> node
 (define (call node value)
   (let ([clos (symbol->closure (cont:closure node))])
     ((vector-ref clos 0) clos value)))

 (define (get-values node)
   (cont:support node))

 (define (get-scores node)
   (cont:scores node))

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
   ;; (display ".")
   (let ([new-graph (step graph)])
     (if (false? new-graph)
         graph
         (explode new-graph))))

 ;; init: thunk -> graph
 (define (init thunk)
   (node->graph (thunk)))

 (define (add-root graph)
   (let ([new-root-cont (make-cont 'root '(#t) '(1.0))]
         [old-root-cont (graph:root graph)])
     (graph:add-node! graph new-root-cont)
     (graph:connect! graph new-root-cont old-root-cont #t 1.0)
     (graph:set-root! graph new-root-cont)
     (graph:set-frontier! graph '())
     graph))

 ;; nodes are uniquely identified by their value
 (define (marginalize graph n)
   (assert (null? (graph:frontier graph)))
   (let ([root (graph:root graph)])
     (define (root? node)
       (requal? node root))
     (define score
       (mem
        (lambda (node t)
          (if (or (= t 0)
                  (root? node))
              1.0
              (let ([conns (graph:parent-conns graph node)])
                (if (null? conns)
                    1.0
                    (sum (map (lambda (conn)
                                (* (conn->score conn)
                                   (score (conn->node conn) (- t 1))))
                              conns))))))))
     (define seen '())
     (define seen?
       (lambda (node t)
         (let* ([key (pair t node)]
                [val (find (lambda (x) (requal? x key)) seen)])
           (when (eq? val #f)
                 (set! seen (cons key seen)))
           val)))
     (define scores '())
     (define frontier (make-frontier))
     (define (make-frontier)
       (let ([q (make-empty-queue)])
         (enqueue! q root)
         q))
     (define (store! val p)
       (set! scores (pair (pair val p) scores)))
     (define (display-graph t)
       (pretty-print
        (map (lambda (kv) (score (first kv) t))
             (hash-table->alist (graph:table graph)))))
     (let timestep ([t 1])
       (display "| ")
       (set! scores '())
       (set! frontier (make-frontier))
       (let loop ()
         ;; (display (queue-length frontier))
         (display ".")
         (if (queue-empty? frontier)
             (if (= t n)
                 (begin
                   ;; (display-graph t)
                   scores)
                 (timestep (+ t 1)))
             (let ([node (dequeue! frontier)])
               (if (leaf? node)
                   (when (not (assoc node scores))
                         (store! node (score node t)))
                   (for-each (lambda (node) (when (not (seen? node t)) (enqueue-if-new! requal? frontier node)))
                             (graph:children graph node)))
               (loop)))))))

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
         (add-root
          (explode
           (init thunk)))
          1)))

 )