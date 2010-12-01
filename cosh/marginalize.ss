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
         (scheme-tools mem))

 (define (leaf? node)
   (not (continuation? node)))

 (define (get-scorer graph)
   (letrec ([scorer
             (mem
              (lambda (node t)
                (if (or (= t 0)
                        (equal? node (graph:root graph)))
                    1.0
                    (let ([links (graph:parent-links graph node)])
                      (if (null? links)
                          1.0
                          (sum (map (lambda (link)
                                      (* (link->weight link)
                                         (scorer (link->target link) (- t 1))))
                                    links)))))))])
     scorer))

 (define (get-watcher)
   (let ([seen '()])
     (lambda (node t)
       (let* ([key (pair t node)]
              [val (find (lambda (node) (equal? node key)) seen)])
         (when (eq? val #f)
               (set! seen (cons key seen)))
         val))))

 (define/curry (marginalize-graph graph iterations)
   (let ([root (graph:root graph)]
         [score (get-scorer graph)]
         [seen? (get-watcher)])
     (let loop ([t 1]
                [marginal-scores '()]
                [queue (make-queue root)])
       (if (queue-empty? queue)
           (if (= t iterations)
               marginal-scores
               (loop (+ t 1)
                     '()
                     (make-queue root)))
           (let ([node (dequeue! queue)])
             (if (leaf? node)
                 (loop t
                       (pair (pair node (score node t))
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