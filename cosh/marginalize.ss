#!r6rs

;; Given graph, compute marginal probabilities.

;; fixme:
;; - log space

(library

 (cosh marginalize)

 (export marginalize-graph
         marginalize-graph/linsolve)

 (import (rnrs)
         (rnrs mutable-pairs)
         (cosh watcher)
         (cosh continuation)
         (scheme-tools)
         (scheme-tools solve)
         (scheme-tools graph)
         (scheme-tools queue)
         (scheme-tools mem)
         (scheme-tools hash)
         (scheme-tools object-id))

 (define (leaf? node)
   (not (continuation? node)))

 (define (get-scorer graph)
   (letrec ([scorer
             (mem
              (lambda (t node)
                (if (or (= t 0)
                        (equal? node (graph:root graph)))
                    1.0
                    (let ([links (graph:parent-links graph node)])
                      (if (null? links)
                          1.0
                          (sum (map (lambda (link)
                                      (* (link->weight link)
                                         (scorer (- t 1) (link->target link))))
                                    links)))))))])
     scorer))

 (define/curry (marginalize-graph graph iterations)
   (let ([root (graph:root graph)]
         [score (get-scorer graph)]
         [seen? (get-watcher)])
     (pe "marginalization ...\n"
         "iteration " 1 "/" iterations "\n")
     (let loop ([t 1]
                [marginal-scores '()]
                [queue (make-queue root)])
       (if (queue-empty? queue)
           (if (= t iterations)
               marginal-scores
               (begin
                 (pe "iteration " (+ t 1) "/" iterations "\n")
                 (loop (+ t 1)
                       '()
                       (make-queue root))))
           (let ([node (dequeue! queue)])
             (if (leaf? node)
                 (loop t
                       (pair (pair node (score t node))
                             marginal-scores)
                       queue)
                 (begin
                   (for-each (lambda (node)
                               (when (not (seen? node t))
                                     (enqueue-if-new! equal? queue node)))
                             (graph:children graph node))
                   (loop t
                         marginal-scores
                         queue))))))))

 (define/curry (marginalize-graph/linsolve graph)
   (pe "marginalization using solver ...\n")
   (let-values ([(leaf-pointer eqn-generator) (graph->eqn-generator graph)])
     (let ([marginal-values (solve eqn-generator)])
       (pe "looking up leaf values ...\n")
       (let ([nodename->prior (alist->hash-table marginal-values)])
         (map (lambda (leaf-name) (pair (id->object leaf-name) (hash-table-ref/default nodename->prior leaf-name 'unknown)))
              (cdr leaf-pointer))))))

 (define (node->variable-name node)
   (if (continuation? node)
       (continuation:closure-id node)
       (object->id node)))

 (define (graph->eqn-generator graph)
   (let ([root (graph:root graph)]
         [seen? (get-watcher)])
     (let ([queue (make-queue root)]
           [leaf-pointer (cons 'leaves '())])
       (values
        leaf-pointer
        (lambda ()
          (if (queue-empty? queue)
              '()
              (let ([node (dequeue! queue)])
                ;; add children to queue
                (if (leaf? node)
                    (set-cdr! leaf-pointer (cons (node->variable-name node) (cdr leaf-pointer)))
                    (map (lambda (node)
                           (when (not (seen? node 1))
                                 (enqueue-if-new! equal? queue node)))
                         (graph:children graph node)))
                ;; return equation: marginal prior (expected number of
                ;; visits) of node is weighted sum of parent priors
                `(= ,(node->variable-name node)
                    ,(if (equal? node (graph:root graph))
                         1.0
                         (let ([links (graph:parent-links graph node)])
                           (if (null? links)
                               1.0
                               `(+ ,@(map (lambda (link) `(* ,(link->weight link)
                                                        ,(node->variable-name (link->target link))))
                                          links)))))))))))))

 )