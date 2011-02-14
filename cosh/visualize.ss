#!r6rs

(library

 (cosh visualize)

 (export visualize-graph
         visualize-sampling)

 (import (rnrs)
         (only (scheme-tools math) sample-discrete)
         (only (scheme-tools srfi-compat :1) filter-map)
         (cosh continuation)
         (scheme-tools)
         (scheme-tools watcher)
         (scheme-tools graph utils)
         (scheme-tools graph)
         (scheme-tools object-id)
         (scheme-tools ubigraph))

 (define history-contexts #f)

 (define vis-trace 20)
 
 (define hex-alphabet "0123456789abcdef") 

 ;; (let ([id (vector-ref (continuation:closure node) 1)])
 ;;   (when print-ids
 ;;         (pe id ": \n")
 ;;         (pretty-print (id->object id))
 ;;         (pe "\n")
 ;;         (number->string id))
 ;;   "")) 
 (define (node-label node-id print-ids)
   "")

 (define (node->id node history)
   (if history-contexts
       (object->id history)
       (object->id node)))

 (define (readable-booleans obj)
   (cond [(pair? obj) (pair (readable-booleans (car obj))
                            (readable-booleans (cdr obj)))]
         [(eq? obj #t) 1]
         [(eq? obj #f) 0]
         [else obj]))

 (define (edge->id from to label weight)
   (object->id (list from to label weight)))

 (define (history->edge-id history label weight)
   (object->id (list history label weight)))
 
 (define (draw-internal node-id)
   (ubi-node node-id (node-label node-id #f))
   (ubi-node-attrib node-id "shape" "cube"))

 (define (draw-leaf node-id node-lst)
   (if (null? node-lst)
       (ubi-node node-id (->string node-id))
       (ubi-node node-id (->string (readable-booleans (car node-lst)))))
   (ubi-node-attrib node-id "shape" "sphere")
   (ubi-node-attrib node-id "color" "#00ff00")
   (ubi-node-attrib node-id "size" "2.0"))
 
 (define (draw-root node-id)
   (ubi-node node-id "")
   (ubi-node-attrib node-id "shape" "sphere")
   (ubi-node-attrib node-id "color" "#ff0000")
   (ubi-node-attrib node-id "size" "2.0"))

 (define (draw-node node-id node-type . maybe-node)
   (cond [(eq? node-type 'root) (draw-root node-id)]
         [(eq? node-type 'leaf) (draw-leaf node-id maybe-node)]
         [else (draw-internal node-id)]))

 (define (draw-edge edge-id from-id to-id label weight)
   (ubi-id-edge edge-id
                from-id
                to-id
                (string-append (->string (readable-booleans label)) " p=" (->string:n weight 4)))
   (ubi-id-edge-attrib edge-id
                       "oriented"
                       "true")
   (ubi-id-edge-attrib edge-id
                       "fontcolor"
                       "#aaaaaa")
   (ubi-id-edge-attrib edge-id
                       "arrow"
                       "true")
   (ubi-id-edge-attrib edge-id
                       "width"
                       "7"))

 (define (visualize graph node done? interactive)
   (map (lambda (link)
          (let* ([child (link->target link)]
                 [child-type (node->type graph child)]
                 [child-id (node->id child #f)]
                 [edge-id (edge->id node child (link->label link) (link->weight link))])
            (when interactive (read-char))
            (draw-node child-id child-type child)
            (draw-edge edge-id
                       (node->id node #f)
                       (node->id child #f)
                       (link->label link)
                       (link->weight link))
            (when (not (done? child))
                  (visualize graph child done? interactive))))
        (graph:child-links graph node)))
 
 (define (visualize-graph graph interactive)
   (ubi-reset)
   (for-each
    (lambda (root)
      (draw-node (node->id root #f) 'root)
      (visualize graph
                 root
                 (make-watcher)
                 interactive))
    (graph:root-nodes graph)))

 (define (alist-add/replace el lst eql)
   (cons el
         (filter (lambda (kv) (not (eql (car kv) (car el))))
                 lst)))

 (define (alist-add/replace* els lst eql)
   (if (null? els)
       lst
       (alist-add/replace* (cdr els)
                           (alist-add/replace (car els)
                                              lst
                                              eql)
                           eql)))

 (define (multinomial-draw vals weights)
   (list-ref vals (sample-discrete weights)))

 (define (sample-link graph node)
   (let* ([links (graph:child-links graph node)]
          [scores (map link->weight links)])
     (if (null? links)
         #f
         (multinomial-draw links scores))))

 (define (dec->hex n)
   (let ([h1 (div n 16)]
         [h2 (mod n 16)])
     (list->string (list (string-ref hex-alphabet h1)
                         (string-ref hex-alphabet h2)))))

 (define (hex r g b sat)
   (apply string-append
          (cons "#" (map (lambda (x) (dec->hex (inexact->exact (div (max x (min sat 100)) 1))))
                         (list r g b)))))
 
 (define (make-root-node-salience-task node-id salience)
   (make-countdown-task
    (lambda (node-id salience)
      (ubi-node-attrib node-id
                       "color"
                       (hex 255 0 0 (* 255 (/ salience vis-trace)))))
    node-id
    salience))

 (define (make-leaf-node-salience-task node-id salience)
   (make-countdown-task
    (lambda (node-id salience)
      (ubi-node-attrib node-id
                       "color"
                       (hex 0 255 0 (* 255 (/ salience vis-trace)))))
    node-id
    salience))

 (define (make-internal-node-salience-task node-id salience)
   (make-countdown-task
    (lambda (node-id salience)
      (ubi-node-attrib node-id
                       "color"
                       (hex 30 30 255 (* 255 (/ salience vis-trace)))))
    node-id
    salience))

 (define (make-link-salience-task link-id salience)
   (make-countdown-task
    (lambda (link-id salience)
      (ubi-id-edge-attrib link-id
                          "color"
                          (hex 0 0 255 (* 255 (/ salience vis-trace)))))
    link-id
    salience))

 (define (make-countdown-task setter obj-id val)
   (pair obj-id
         (lambda ()
           (if (= val 0)
               #f
               (begin
                 (setter obj-id val)
                 (make-countdown-task setter obj-id (- val 1)))))))

 (define (make-node-salience-task node-type node-id init-salience)
   (cond [(eq? node-type 'root)
          (make-root-node-salience-task node-id vis-trace)]
         [(eq? node-type 'leaf)
          (make-leaf-node-salience-task node-id vis-trace)]
         [else (make-internal-node-salience-task node-id vis-trace)]))

 ;; Call draw-node, draw-edge, tasks with node ids compute node ids
 ;; from both nodes and histories add flag that distinguishes between
 ;; the two.

 (define (node->type graph node)
   (cond [(graph:root? graph node) 'root]
         [(graph:leaf? graph node) 'leaf]
         [else 'internal]))
 
 (define (%visualize-sampling graph node wait background-tasks history)
   (let* ([filtered-tasks (filter-map (compose (lambda (f) (f)) cdr)
                                      background-tasks)]
          [link (sample-link graph node)])
     (if (not link)
         (%visualize-sampling-start graph
                                    wait
                                    filtered-tasks)
         (let* ([new-history (cons (link->label link) history)]
                [child (link->target link)]
                [child-type (node->type graph child)]
                [child-id (node->id child new-history)]
                [link-id (if history-contexts
                             (history->edge-id history (link->label link) (link->weight link))
                             (edge->id node child (link->label link) (link->weight link)))])
           (draw-node child-id child-type child)
           (draw-edge link-id
                      (node->id node history)
                      child-id
                      (link->label link)
                      (link->weight link))
           (ubi-node-attrib child-id "color" "#ffffff")
           (ubi-id-edge-attrib link-id "color" "#ffffff")
           (let* ([node-task (make-node-salience-task child-type child-id vis-trace)]
                  [link-task (make-link-salience-task link-id vis-trace)]
                  [new-tasks (alist-add/replace* (list node-task link-task)
                                                 filtered-tasks
                                                 equal?)])
             (when (not (= wait 0.0))
                   (time-wait wait))
             (%visualize-sampling graph child wait new-tasks new-history))))))

 (define (%visualize-sampling-start graph wait background-tasks)
   (let* ([root-node (graph:root graph)]
          [root-id (node->id root-node '())])
     (time-wait 1)
     (draw-node root-id 'root)
     (time-wait wait)
     (%visualize-sampling graph
                          root-node
                          wait
                          (alist-add/replace
                           (make-node-salience-task 'root root-id vis-trace)
                           background-tasks
                           equal?)
                          '())))

 (define (visualize-sampling graph wait)
   (ubi-reset)
   (%visualize-sampling-start graph wait '()))

 )