#!r6rs

;; Given graph, compute marginal probabilities.

;; fixme:
;; - log space

(library

 (cosh marginalize)

 (export marginalize-graph)

 (import (rnrs)
         (cosh continuation)
         (scheme-tools)
         (scheme-tools graph)
         (scheme-tools queue)
         (scheme-tools mem)
         (scheme-tools hash))

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

 (define (get-watcher)
   (let ([time-tables (make-hash-table)])
     (lambda (node t)
       (let ([time-table (hash-table-ref time-tables
                                         t
                                         (lambda () (let ([table (make-hash-table)])
                                                 (hash-table-set! time-tables t table)
                                                 table)))])
         (let ([seen (hash-table-ref/default time-table
                                             node
                                             #f)])
           (when (eq? seen #f)
                 (hash-table-set! time-table node #t))
           seen)))))

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

 )