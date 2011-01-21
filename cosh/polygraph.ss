#!r6rs

;; Sketch (untested!)

;; Given return-thunk, build polygraph (which corresponds to a system
;; of polynomial equations).


;; --------------------------------------------------------------------
;; Utilities

;; When we build the polynomial, we need to distinguish marginal
;; probabilities of terminals based on what root node they are
;; associated with. A score-ref references such a marginal
;; probability.
(define (make-score-ref root-node value)
  (list 'ref (object-id root-node) value))

(define (make-root-node id)
  (make-continuation id '(init) '(1.0)))

(define top-cont-closure
  (vector (lambda (self top-value) top-value)
   'top))


;; --------------------------------------------------------------------
;; Graph library extension
;; 
;; (graph:reachable-terminals graph node)
;;     Return all existing terminal nodes in graph reachable from node.
;; 
;; (graph:register-callback! node callback)
;;     Associate function callback with node.
;;
;; (graph:notify-ancestors-of-connection! graph node last-node)
;;     For each terminal reachable from node, call the ancestor
;;     callbacks of last-node. This is used when a node (last-node) is
;;     connected to an existing node that may already have known
;;     terminal values.
;;
;; (graph:add/retrieve! graph node)
;;     Create node if new, return is-new boolean.
;;
;; (graph:add/link! graph last-node node link-promise)
;;     Create node if new, link to last-node, return is-new boolean.
;;
;; FIXME: When graph:reachable-terminals and graph:ancestor-callbacks
;; are called many times on the same graph (but different nodes), they
;; do too much work. How can we build up this information
;; incrementally?

(define traversal-memory (make-parameter #f))

(define callback-registry (make-parameter #f))

(define (traverse start next combine stop? default)
  (if (stop? start)
      default
      (combine start
               (map (lambda (obj) (traverse obj next combine stop? default))
                    (next start)))))

(define (seen? node)
  (hash-table-ref (traversal-memory)
                  node
                  (lambda () (begin
                          (hash-table-set! (traversal-memory) node #t)
                          #f))))

(define (terminal-value? graph node)
  (and (not (continuation? node))
       (not (application? node))
       (graph:leaf? graph node)))

(define (graph:reachable-terminals graph node)
  (parameterize
   ([traversal-memory (make-finitize-hash-table)])  
   (traverse node
             (lambda (node) (graph:children graph node))
             (lambda (node list-of-terminals)
               (if (terminal-value? graph node)
                   (cons node list-of-terminals)
                   list-of-terminals))
             seen?
             '())))

(define (graph:register-callback! node callback)
  (hash-table-set! (callback-registry) node (pair callback (graph:callbacks node))))

(define (graph:callbacks node)
  (hash-table-ref/default (callback-registry) node '()))

(define (graph:ancestor-callbacks graph node)
  (parameterize
   ([traversal-memory (make-finitize-hash-table)])
   (traverse node
             (lambda (node) (graph:parents graph node))
             (lambda (node list-of-callback-lists)
               (append (graph:callbacks node) (apply append list-of-callback-lists)))
             seen?
             '())))

(define (graph:notify-ancestors-of-connection! graph node last-node)
  (let ([terminals (graph:reachable-terminals graph node)])
    (map (lambda (callback) (map (lambda (terminal) (callback terminal)) terminals))
         (graph:ancestor-callbacks graph last-node))))

(define (graph:add/retrieve! graph node)
  (let* ([is-new (not (graph:node-exists? graph node))])
    (when is-new
          (graph:add-node! graph node))
    is-new))

(define (graph:add/link! graph last-node node link-promise)
  (let* ([is-new (not (graph:node-exists? graph node))]
         [connect! (if is-new graph:add-child! graph:link!)])
    (connect! graph
             last-node
             node
             (link-promise->label link-promise)
             (link-promise->weight link-promise))
    is-new))


;; --------------------------------------------------------------------
;; Data structures

(define (make-link-promise weight label)
  (list 'link-promise weight label))

(define link-promise->weight second)

(define link-promise->label third)


;; --------------------------------------------------------------------
;; Main logic
;;
;; FIXME: Make sure we are not doing duplicate work (make callbacks
;; remember which values they have seen?)

;; Make graph, create a root node in graph, new callback registry,
;; then call build-graph.
(define (build-graph:top thunk)
  (let ([graph (make-graph)]
        [root-node (make-root-node 'root)]
        [root-link-promise (make-link-promise 1.0 #t)])
    (graph:add-node! graph root-node)
    (parameterize
     ([callback-registry (make-finitize-hash-table)])    
     (build-graph graph thunk root-node root-link-promise))))

;; Create/retrieve graph node for new (thunk) result, connect to last
;; node in graph. Dispatch according to type of node.
(define (build-graph graph thunk last-node link-promise)
  (define (get-handler node)
    (cond [(continuation? node) build-graph:continuation]
          [(app? node) build-graph:application]
          [else build-graph:value]))
  (let* ([node (thunk)]
         [handler (get-handler node)]
         [is-new (graph:add/link! graph last-node node link-promise)])
    (if is-new
        (handler graph node last-node)
        (graph:notify-ancestors-of-connection! graph node last-node))))

;; Notify the callbacks of all ancestors that we found a terminal.
(define (build-graph:value graph node last-node)
  (graph:notify-ancestors-of-connection! graph node last-node))

;; Extend graph by exploring all possible branches of the (xrp)
;; continuation.
(define (build-graph:continuation graph node last-node)
  (map (lambda (value score)
         (build-graph graph
                      (lambda () (call-continuation node value))
                      node
                      (make-link-promise value score)))
       (cont:support node)
       (cont:scores node)))

;; Make thunk for delimited application, root node for application
;; subgraph, callback to continue with outer continuation when
;; terminal values are found; associate callback with application
;; subgraph root.
;; If the delimited application root node is new, build graph from
;; there. If not, just call callback on all existing terminal values.
(define (build-graph:application graph node last-node)
  (let* ([subthunk (lambda () (call-app-with-cont node top-cont-closure))]
         [subroot-node (make-root-node (sym+num 'app (app:delimited-id node)))]
         [subroot-link-promise (make-link-promise 1.0 #t)]
         [subroot-is-new (graph:add/retrieve! graph subroot-node)]
         [marginal-score-ref (make-score-ref subroot-node value)]
         [callback (lambda (value)
                     (build-graph graph
                                  (lambda () (call-app-cont node value))
                                  node
                                  (make-link-promise value marginal-score-ref)))])
    (graph:register-callback! subroot-node callback)
    (if subroot-is-new
        (build-graph graph subthunk subroot-node subroot-link-promise)
        (map callback (graph:reachable-terminals graph subroot-node)))))
