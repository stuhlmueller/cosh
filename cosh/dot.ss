#!r6rs

(library

 (cosh dot)

 (export polygraph->dot
         polygraph->file)

 (import (rnrs)
         (cosh polycommon)
         (scheme-tools)
         (scheme-tools object-id)
         (scheme-tools srfi-compat :1)
         (scheme-tools graph))

 (define generate-graph-id
   (let ([counter (get-counter)])
     (lambda ()
       (number->string (+ (counter) 10000)))))

 (define (node->name graph node)
   (if (graph:leaf? graph node)
       (->string node)
       (node->graph-id graph node)))

 (define (node->graph-id graph node)
   (number->string (object->id node)))
 
 (define (node->dot graph node)
   (string-append (node->graph-id graph node)
                  " ["
                  "label=\"" (node->name graph node) "\""
                  "]\n"))

 (define (readable-number n)
   (->string:n n 5))

 ;; If the target is a leaf node, don't link directly to it but
 ;; introduce a new node in the graph in order to make the resulting
 ;; graph easier to parse visually.
 (define (link->dot graph node link)
   (let* ([target-is-leaf (graph:leaf? graph (link->target link))]
          [target-graph-id (if target-is-leaf
                               (generate-graph-id)
                               (node->graph-id graph (link->target link)))])
     (string-append
      (if target-is-leaf
          (string-append target-graph-id
                         " ["
                         "label=\"" (node->name graph (link->target link)) "\""
                         "]\n")
          "")
      (let* ([weight (link->weight link)])
        (string-append (node->graph-id graph node)
                       " -> "
                       target-graph-id
                       " ["
                       "label=\""
                       (if (score-ref? weight)
                           (string-append (node->name graph (score-ref->root weight)) ":"
                                          (node->name graph (score-ref->terminal-node weight)))
                           (readable-number weight))
                        "\""                  
                       "]\n")))))

 (define (entry->dot graph entry)
   (let ([node (first entry)]
         [links (rest entry)])
     (apply string-append
            (pair (node->dot graph node)
                  (map (lambda (link) (link->dot graph node link))
                       links)))))

 (define (polygraph->dot graph)
   (apply string-append
          `("digraph G {\n"
            ,@(map (lambda (entry) (entry->dot graph entry))
                   (graph->alist graph))
            "\n}\n\n")))

 (define (polygraph->file graph)
   (pe "Writing graph to /tmp/graph.dot\n")
   (system "rm /tmp/graph.dot")
   (with-output-to-file "/tmp/graph.dot"
     (lambda () (display (polygraph->dot graph))))
   graph)

 )