#!r6rs

;; Reduce polygraph to a form without immediate score-ref redirects
;; and single-erp subproblems (i.e. with all problems reduced away
;; where marginal probability can be read off graph).

;; Idea:
;; - Go through whole graph once to make hash table that maps root
;;   nodes to immediate marginal probabilities / score-refs.
;; - Iteratively look up & replace score-refs in hash table until no
;;   changes happen anymore.
;; - Go through whole graph a second time, replace scores/score-refs
;;   with entries in hash table.

(library
 
 (cosh polygraph simplify)
 
 (export simplify-polygraph!)
 
 (import (cosh polycommon)
         (rnrs)
         (scheme-tools)
         (scheme-tools graph utils)
         (scheme-tools graph)
         (scheme-tools hash)
         (scheme-tools mem)
         (scheme-tools watcher)
         (scheme-tools readable-scheme)
         (scheme-tools srfi-compat :1))

 (define not-found (gensym))

 (define (found? obj)
   (not (eq? obj not-found)))

 ;; Table that maps pairs of root nodes and terminal values to
 ;; marginals in cases where the marginal probability (or score-ref)
 ;; can be read off the graph.
 (define (make-marginal-table graph)
   (let ([root-nodes (graph:root-nodes graph)]
         [marginal-table (make-finitize-hash-table)]
         [seen? (make-watcher)])
     (for-each (lambda (root) (let* ([local-table (make-finitize-hash-table)]
                                [success (call/cc (lambda (cont) (find-marginals graph root root local-table seen? cont)))])
                           (when success
                                 (hash-table-merge! marginal-table local-table))))
               root-nodes)
     marginal-table))

 ;; If possible, compute an explicit representation of the total
 ;; weight given the stored weight (or false) and the new weight. This
 ;; is possible if (1) no old weight exists or (2) both old and new
 ;; weight exist and are numbers (not score-refs).
 (define (update-weight stored-weight new-weight stop)
   (cond [(not stored-weight) new-weight]
         [(and (number? stored-weight) (number? new-weight))
          (+ stored-weight new-weight)]
         [else (stop #f)]))

 ;; For each root node in the graph, follow all paths downwards. If
 ;; probability is 1, continue. Otherwise, if target is a terminal
 ;; node, stop and store prob/score-ref for pair of root node and
 ;; terminal node. If target is not a terminal node, give up on this
 ;; subproblem (need to throw out all marginal info, since more
 ;; complicated paths could lead to simple marginal values).
 (define (find-marginals graph root node table seen? stop)
   (let ([child-links (graph:child-links graph node)])
     (for-each (lambda (link)
                 (when (not (seen? (pair node link)))
                       (if (graph:leaf? graph (link->target link))
                           (let* ([index (pair root (link->target link))]
                                  [stored-weight (hash-table-ref/default table index #f)]
                                  [new-weight (update-weight stored-weight (link->weight link) stop)])
                             (hash-table-set! table index new-weight))
                           (if (equal? (link->weight link) 1.0)
                               (find-marginals graph root (link->target link) table seen? stop)
                               (stop #f)))))
               child-links)))

 ;; (proc, table) -> changed
 (define (hash-table-map! proc table equality)
   (let ([changed #f])
     (let-values (((keys values) (hashtable-entries table)))     
       (vector-map (lambda (key value)
                     (let ([new-value (proc value)])
                       (when (not (equality value new-value))
                             (hash-table-set! table key new-value)
                             (set! changed #t))))
                   keys
                   values)
       changed)))

 (define (iterate! table proc)
    (let ([changed (hash-table-map! proc table finitize-equal?)])
     (if changed
         (iterate! table proc)
         table)))

 ;; Given a table that maps pairs of root nodes and terminals to
 ;; marginal probabilities, this returns a function that takes in a
 ;; value and replaces it with its marginal probability if it is a
 ;; score-ref.  Both "value" and "stored-weight" can be either a
 ;; score-ref or a number.
 (define (get-marginal-updater table)
   (lambda (value)
     (if (score-ref? value)
         (let ([stored-weight
                (hash-table-ref/default table
                                        (pair (score-ref->root value)
                                              (score-ref->terminal-node value))
                                        not-found)])
           (if (found? stored-weight)
               stored-weight
               value))
         value)))

 ;; Given a table that maps pairs of root nodes and terminals to
 ;; marginal probabilities, this returns a function that maps links to
 ;; links where score-refs are resolved one step further.
 (define (get-link-updater table)
   (let ([weight-updater (get-marginal-updater table)])
     (lambda (link)
       (make-link (link->label link)
                  (weight-updater (link->weight link))
                  (link->target link)))))
          
 (define entry->node first)

 ;; Identify link by label.
 (define (graph:update-link! graph node link)
   (graph:link! graph
                node
                (link->target link)
                (link->label link)
                (link->weight link)
                #t))

 ;; Given a graph and a table of marginal probabilities, this resolves
 ;; all score-refs one step further.
 ;; FIXME: can we use entry->links instead of graph:child-links? 
 (define (update-weights! graph table)
   (let* ([link-updater (get-link-updater table)])
     (for-each (lambda (entry)
                 (let ([node (entry->node entry)])
                   (for-each (lambda (link) (graph:update-link! graph node (link-updater link)))
                             (graph:child-links graph node))))
               (graph->alist graph))))

 (define (graph-walk graph node proc)
   (let ([seen? (make-watcher)])
     (let loop ([node node])
       (proc node)
       (let ([child-links (graph:child-links graph node)])
         (for-each (lambda (link)
                     (let ([weight (link->weight link)])
                       (when (and (score-ref? weight)
                                  (not (seen? (score-ref->root weight))))
                             (loop (score-ref->root weight)))
                       (when (not (seen? (link->target link)))
                             (loop (link->target link)))))
                   child-links)))))

 ;; 1. Traverse graph from root, following score-refs, remember all nodes.
 ;; 2. Check for each node whether we saw it.
 (define (remove-unreachable-nodes! graph)
   (let ([reachable? (make-watcher)])
     (graph-walk graph (graph:root graph) reachable?)
     (let ([frozen-reachable? (mem reachable?)])
       (for-each (lambda (entry)
                   (let ([node (entry->node entry)])
                     (when (not (frozen-reachable? node))
                           (graph:remove-node! graph node))))
                 (graph->alist graph))
       graph)))
 
 (define (simplify-polygraph! graph)
   (let* ([base-table (make-marginal-table graph)]
           [table (iterate! base-table (get-marginal-updater base-table))])
     (update-weights! graph table)
     (remove-unreachable-nodes! graph)
     graph))

 )