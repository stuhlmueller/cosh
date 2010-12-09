#!r6rs

(library

 (cosh visualize)

 (export visualize-graph)

 (import (rnrs)
         (cosh continuation)
         (cosh watcher)
         (scheme-tools)
         (scheme-tools graph)
         (scheme-tools object-id)
         (scheme-tools ubigraph))

 (define (node-label node)
   (let ([id (vector-ref (continuation:closure node) 1)])
     ;; (pe id ": \n")
     ;; (pretty-print (id->object id))
     ;; (pe "\n")
     ;; (number->string id)
     ""))
 
 (define (draw-internal node)
   (ubi-node (object->id node) (node-label node))
   (ubi-node-attrib (object->id node) "shape" "cube"))

 (define (draw-leaf node)
   (ubi-node (object->id node) (->string node))
   (ubi-node-attrib (object->id node) "shape" "sphere")
   (ubi-node-attrib (object->id node) "color" "#00ff00")
   (ubi-node-attrib (object->id node) "size" "2.0"))

 (define (draw-root node)
   (ubi-node (object->id node) "")
   (ubi-node-attrib (object->id node) "shape" "sphere")
   (ubi-node-attrib (object->id node) "color" "#ff0000")
   (ubi-node-attrib (object->id node) "size" "2.0"))

 (define (draw-node graph node)
   (cond [(graph:root? graph node) (draw-root node)]
         [(graph:leaf? graph node) (draw-leaf node)]
         [else (draw-internal node)]))
 
 (define (draw-edge from to label weight)
   (ubi-edge (object->id from)
             (object->id to)
             (string-append (->string label) " " (->string:n weight 4)))
   (ubi-edge-attrib "oriented"
                    "true")
   (ubi-edge-attrib "fontcolor"
                    "#aaaaaa")
   (ubi-edge-attrib "arrow"
                    "true")
   (ubi-edge-attrib "width"
                    "2"))

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

 )