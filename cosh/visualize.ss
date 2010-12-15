#!r6rs

(library

 (cosh visualize)

 (export visualize-graph
         visualize-sampling)

 (import (rnrs)
         (only (church external math-env) sample-discrete)
         (only (_srfi :1) filter-map)
         (cosh continuation)
         (cosh watcher)
         (scheme-tools)
         (scheme-tools graph)
         (scheme-tools object-id)
         (scheme-tools ubigraph))

 (define vis-trace 20)
 
 (define hex-alphabet "0123456789abcdef") 

 (define (node-label node print-ids)
   (let ([id (vector-ref (continuation:closure node) 1)])
     (when (print-ids)
           (pe id ": \n")
           (pretty-print (id->object id))
           (pe "\n")
           (number->string id))
     ""))
 
 (define (draw-internal node)
   (let ([node-id (object->id node)])
     (ubi-node node-id (node-label node #f))
     (ubi-node-attrib node-id "shape" "cube")
     node-id))

 (define (draw-leaf node)
   (let ([node-id (object->id node)])
     (ubi-node node-id (->string node))
     (ubi-node-attrib node-id "shape" "sphere")
     (ubi-node-attrib node-id "color" "#00ff00")
     (ubi-node-attrib node-id "size" "2.0")
     node-id))

 (define (draw-root node)
   (let ([node-id (object->id node)])
     (ubi-node node-id "")
     (ubi-node-attrib node-id "shape" "sphere")
     (ubi-node-attrib node-id "color" "#ff0000")
     (ubi-node-attrib node-id "size" "2.0")
     node-id))

 (define (draw-node graph node)
   (cond [(graph:root? graph node) (draw-root node)]
         [(graph:leaf? graph node) (draw-leaf node)]
         [else (draw-internal node)]))
 
 (define (draw-edge from to label weight)
   (let ([edge-id (object->id (list from to label weight))])
     (ubi-id-edge edge-id
                  (object->id from)
                  (object->id to)
                  (string-append (->string label) " " (->string:n weight 4)))
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
                         "4")
     edge-id))

 (define (visualize graph node done? interactive)
   (map (lambda (link)
          (let ([child (link->target link)])
            (when interactive (read-char))
            (draw-node graph child)
            (draw-edge node child (link->label link) (link->weight link))
            (when (not (done? child))
                  (visualize graph child done? interactive))))
        (graph:child-links graph node)))
 
 (define (visualize-graph graph interactive)
   (ubi-reset)
   (draw-node graph (graph:root graph))
   (visualize graph
              (graph:root graph)
              (get-watcher)
              interactive))

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
   (let ([m (/ sat 255.0)])
     (apply string-append
            (cons "#" (map (lambda (x) (dec->hex (inexact->exact (div (+ (* .3 x) (* .7 x m)) 1))))
                           (list r g b))))))
 
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
                       (hex 0 0 255 (* 255 (/ salience vis-trace)))))
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

 (define (make-node-salience-task graph node node-id init-salience)
   (cond [(graph:root? graph node)
          (make-root-node-salience-task node-id vis-trace)]
         [(graph:leaf? graph node)
          (make-leaf-node-salience-task node-id vis-trace)]
         [else (make-internal-node-salience-task node-id vis-trace)]))

 (define (%visualize-sampling graph node wait background-tasks)
   (let* ([filtered-tasks (filter-map (compose (lambda (f) (f)) cdr)
                                      background-tasks)]
          [link (sample-link graph node)])
     (if (not link)
         (%visualize-sampling-start graph
                                    wait
                                    filtered-tasks)
         (let* ([child (link->target link)]
                [child-id (draw-node graph child)]
                [link-id (draw-edge node
                                    child
                                    (link->label link)
                                    (link->weight link))])
           (ubi-node-attrib child-id "color" "#ffffff")
           (ubi-id-edge-attrib link-id "color" "#ffffff")
           (let* ([node-task (make-node-salience-task graph child child-id vis-trace)]
                  [link-task (make-link-salience-task link-id vis-trace)]
                  [new-tasks (alist-add/replace* (list node-task link-task)
                                                 filtered-tasks
                                                 equal?)])
             (when (not (= wait 0.0))
                   (time-wait wait))
             (%visualize-sampling graph child wait new-tasks))))))

 (define (%visualize-sampling-start graph wait background-tasks)
   (let* ([root-node (graph:root graph)]
          [root-id (draw-node graph root-node)])
     (time-wait wait)
     (%visualize-sampling graph
                          root-node
                          wait
                          (cons (make-node-salience-task graph root-node root-id vis-trace)
                                background-tasks))))

 (define (visualize-sampling graph wait)
   (ubi-reset)
   (%visualize-sampling-start graph wait '()))

 )