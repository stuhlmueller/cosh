#!r6rs

;; Utilities used by the polynomial graph-building and
;; equation-generating part of cosh.

(library

 (cosh polycommon)

 (export graph:reachable-terminals
         graph:notify-ancestors-of-connection!
         node:id
         score-ref->terminal-node
         score-ref->root
         score-ref?
         make-score-ref
         link-promise->label
         link-promise->weight
         make-link-promise
         terminal-value?
         value?
         make-root-node
         identity-cont-closure)

 (import (rnrs)
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

 (define (make-root-node id)
   (make-continuation id '(init) '(0.0)))

 (define identity-cont-closure
   (vector (lambda (self top-value) top-value)
           'top))

 (define (node:id node)
   (cond [(continuation? node) (continuation:id node)]
         [(application? node) (application:id node)]
         [else (object->id node)]))

 (define (value? obj)
   (and (not (continuation? obj))
        (not (application? obj))))

 (define (terminal-value? graph node)
   (and (value? node)
        (graph:leaf? graph node)))

 (define (graph:reachable-terminals graph node)
   (traverse node
             (lambda (node) (graph:children graph node))
             (lambda (node list-of-terminals)
               (if (terminal-value? graph node)
                   (cons node list-of-terminals)
                   (apply append list-of-terminals)))
             (make-watcher)
             '()))

 (define (graph:terminals&callbacks graph node last-node)
   (let ([terminals (graph:reachable-terminals graph node)]
         [callbacks (graph:ancestor-callbacks graph last-node)])
     (values terminals callbacks)))

 (define (graph:notify-ancestors-of-connection! graph node last-node)
   (let-values ([(terminals callbacks) (graph:terminals&callbacks graph node last-node)])
     (map (lambda (callback) (map (lambda (terminal) (callback terminal)) terminals))
          callbacks)))

 (define (make-score-ref root-node terminal-node)
   (list 'score-ref root-node terminal-node))

 (define (score-ref? obj)
   (tagged-list? obj 'score-ref))

 (define score-ref->root second)

 (define score-ref->terminal-node third)

 (define (make-link-promise weight label)
   (list 'link-promise weight label))

 (define link-promise->weight second)

 (define link-promise->label third)

 )