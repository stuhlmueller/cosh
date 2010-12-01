#!r6rs

;; Given graph, compute marginal probabilities.

;; fixme:
;; - log space

(library

 (cosh marginalize)

 (export marginalize)

 (import (rnrs)
         (transforms)
         (cosh continuation)
         (scheme-tools)
         (scheme-tools graph)
         (scheme-tools queue)
         (scheme-tools hash)
         (scheme-tools mem))

 (define (value? node)
   (not (continuation? node)))

 (define leaf? value?) 

 ;; nodes are uniquely identified by their value
 (define (marginalize graph n)
   (let ([root (graph:root graph)])
     (define (root? node)
       (requal? node root))
     (define score
       (mem
        (lambda (node t)
          (if (or (= t 0)
                  (root? node))
              1.0
              (let ([links (graph:parent-links graph node)])
                (if (null? links)
                    1.0
                    (sum (map (lambda (link)
                                (* (link->weight link)
                                   (score (link->target link) (- t 1))))
                              links))))))))
     (define seen '())
     (define seen?
       (lambda (node t)
         (let* ([key (pair t node)]
                [val (find (lambda (x) (requal? x key)) seen)])
           (when (eq? val #f)
                 (set! seen (cons key seen)))
           val)))
     (define scores '())
     (define (store! val p)
       (set! scores (pair (pair val p) scores)))
     (define queue (make-queue root))
     (let timestep ([t 1])
       (display "| ")
       (set! scores '())
       (set! queue (make-queue root))
       (let loop ()
         (if (queue-empty? queue)
             (if (= t n)
                 scores
                 (timestep (+ t 1)))
             (let ([node (dequeue! queue)])
               (if (leaf? node)
                   (when (not (assoc node scores))
                         (store! node (score node t)))
                   (for-each (lambda (node) (when (not (seen? node t)) (enqueue-if-new! requal? queue node)))
                             (graph:children graph node)))
               (loop)))))))

 )