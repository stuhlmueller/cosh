#!r6rs

;; Given return-thunk, build polygraph (which corresponds to a system
;; of polynomial equations).

(library

 (cosh polygraph polygraph)

 (export return-thunk->polygraph)

 (import (rnrs)
         (cosh polycommon)
         (cosh continuation)
         (cosh application)
         (scheme-tools)
         (scheme-tools object-id)
         (scheme-tools hash)
         (scheme-tools mem)
         (scheme-tools watcher)
         (scheme-tools graph)
         (scheme-tools graph callback)
         (scheme-tools graph utils)
         (scheme-tools srfi-compat :1))

 ;; Make graph, create a root node in graph, new callback registry,
 ;; then call build-graph.
 (define (return-thunk->polygraph thunk graph-size-limit)
   (let ([graph (make-graph)]
         [root-node (make-root-node 'root)]
         [root-link-promise (make-link-promise 1.0 #t)])
     (graph:add-node! graph root-node)
     (graph:set-root! graph root-node)
     (build-graph graph graph-size-limit thunk root-node root-link-promise)
     graph))

 ;; Create/retrieve graph node for new (thunk) result, connect to last
 ;; node in graph. Dispatch according to type of node.
 (define (build-graph graph graph-size-limit thunk last-node link-promise)
   (define (get-handler node)
     (cond [(continuation? node) build-graph:continuation]
           [(application? node) build-graph:application]
           [else build-graph:value]))
   (when
    (or (not graph-size-limit)
        (<= (graph-size graph) graph-size-limit))
    (let* ([node (thunk)]
           [handler (get-handler node)]
           [is-new (graph:add/link! graph last-node node
                                    (link-promise->label link-promise)
                                    (link-promise->weight link-promise))])
      (if is-new
          (handler graph graph-size-limit node last-node)
          (graph:notify-ancestors-of-connection! graph node last-node)))))

 ;; Notify the callbacks of all ancestors that we found a terminal.
 (define (build-graph:value graph graph-size-limit node last-node)
   (graph:notify-ancestors-of-connection! graph node last-node))

 ;; Extend graph by exploring all possible branches of the (xrp)
 ;; continuation.
 (define (build-graph:continuation graph graph-size-limit node last-node)
   (map (lambda (value score)
          (build-graph graph
                       graph-size-limit
                       (lambda () (call-continuation node value))
                       node
                       (make-link-promise score value)))
        (continuation:support node)
        (continuation:scores node)))

 ;; Make thunk for delimited application, root node for application
 ;; subgraph, callback to continue with outer continuation when
 ;; terminal values are found; associate callback with application
 ;; subgraph root. If the delimited application root node is new,
 ;; build graph from there. If not, just call callback on all existing
 ;; terminal values.
 (define (build-graph:application graph graph-size-limit node last-node)
   (let* ([subthunk (lambda () (call-application-with-cont node identity-cont-closure))]
          [subroot-node (make-root-node (sym+num 'app (application:delimited-id node)))]
          [subroot-link-promise (make-link-promise 1.0 #t)]
          [subroot-is-new (graph:add/retrieve! graph subroot-node)]
          [pos (gensym)]
          [callback (mem
                     (lambda (value)
                       (build-graph graph
                                    graph-size-limit
                                    (lambda () (call-application-cont node value))
                                    node
                                    (make-link-promise (make-score-ref subroot-node value)
                                                       value))))])
     (graph:register-callback! graph subroot-node callback)
     (if subroot-is-new
         (build-graph graph graph-size-limit subthunk subroot-node subroot-link-promise)
         (map callback (graph:reachable-terminals graph subroot-node)))))

 )