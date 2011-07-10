#!r6rs

;; A polymap is a summary graph of the dependency structure of a
;; polygraph. Contains a node for each subproblem (root
;; node). Whenever a subproblem A references another subproblem B,
;; there is a link from A to B in the polymap. In general, this graph
;; is not acyclic.

(library

 (cosh polymap)

 (export polygraph->polymap)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools graph)
         (scheme-tools watcher)
         (cosh polycommon))

 (define (polygraph->polymap graph)
   (let ([polymap (make-graph)]
         [seen? (make-watcher)])
     (build-polymap! graph seen? polymap (graph:root graph) (graph:root graph))))

 (define (build-polymap! graph seen? polymap root node)
   (when (not (seen? (pair node root)))
         (when (null? (graph:parent-links graph node))
               (graph:add-node! polymap node))
         (for-each (lambda (link)
                     (let ([weight (link->weight link)])
                       (when (score-ref? weight)
                             (let ([subroot (score-ref->root weight)])
                               (build-polymap! graph seen? polymap subroot subroot)
                               (graph:link! polymap root subroot subroot 'unweighted)))
                       (build-polymap! graph seen? polymap root (link->target link))))
                   (graph:child-links graph node)))
   polymap)

 )