#!r6rs

;; Distribution data structure that supports symbolic math

(library

 (cosh simple dist)

 (export dist?
         dist->alist
         dist->entries
         dist-collapse
         dist-mix
         dist-prob
         dist-probs
         dist-prob-defs
         dist-prob-vars
         dist-product
         dist-scale
         dist-sum
         dist-vals
         null-dist?
         null-dist
         dist-mass
         distify
         entries->dist
         entry->prob
         entry->val
         make-dist
         pretty-print-dist
         singleton-dist
         dist-hash-table-maker)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools hash)
         (scheme-tools math symbolic)
         (scheme-tools srfi-compat :1))

 (define dist-hash-table-maker
   (make-parameter make-equal-hash-table))
 
 (define (make-dist vals prob-defs . prob-vars)
   (if (null? prob-vars)
       (list 'dist vals prob-defs #f)
       (list 'dist vals prob-defs (car prob-vars))))

 (define dist-vals second)
 (define dist-prob-defs third)
 (define dist-prob-vars fourth)

 (define (dist-probs dist)
   (let ([vars (dist-prob-vars dist)])
     (if vars
         vars
         (dist-prob-defs dist))))

 (define (dist? obj)
   (and (tagged-list? obj 'dist)
        (or (= (length obj) 3)
            (= (length obj) 4))))
 
 (define (dist-prob d v)
   (let ([dp (assoc v (dist->alist d))])
     (if dp (rest dp) 0.0)))

 
 (define entry->val first)
 (define entry->prob-def second)
 (define entry->prob-var third)
 (define (entry->prob entry)
   (if (= (length entry) 3)
       (entry->prob-var entry)
       (entry->prob-def entry)))

 (define (dist->entries d)
   (assert (dist? d))
   (let ([prob-vars (dist-prob-vars d)])
     (if prob-vars
         (zip (dist-vals d)
              (dist-prob-defs d)
              prob-vars)
         (zip (dist-vals d)
              (dist-prob-defs d)))))         
         
 (define (entries->dist entries)
   (cond [(null? entries) (make-dist '() '())]
         [(all (lambda (e) (= (length e) 3)) entries)
          (make-dist (map first entries)
                     (map second entries)
                     (map third entries))]
         [else (make-dist (map first entries)
                          (map second entries))]))

 (define (dist->alist d)
   (map pair
        (dist-vals d)
        (dist-probs d)))

 (define (pretty-print-dist d)
   (if (dist-prob-vars d)
       (for-each (lambda (v s p)
                   (pe v ": " s " -- " p "\n"))
                 (dist-vals d)
                 (dist-prob-vars d)
                 (dist-prob-defs d))
       (for-each (lambda (v p)
                   (pe "p(" v ") = " p "\n"))
                 (dist-vals d)
                 (dist-prob-defs d))))

 (define (singleton-dist val)
   (make-dist (list val)
              (list 1.0)))

 (define (distify val)
   (if (dist? val)
       val
       (singleton-dist val)))

 (define (dist-product dists f)
   (entries->dist
    (map (lambda (entries)
           (list (f (map entry->val entries))
                 (apply s* (map entry->prob entries))))
         (all-combinations (map dist->entries dists)))))

 (define (alist->entries lst)
   (map (lambda (kv) (list (car kv) (cdr kv)))
        lst))
 
 (define/kw (dist-collapse d)
   (let ([table ((dist-hash-table-maker))])
     (for-each (lambda (v p)
                 (let ([p0 (hash-table-ref/default table v #f)])                
                   (hash-table-set! table
                                    v
                                    (if p0 (s+ p0 p) p))))
               (dist-vals d)
               (dist-probs d))
     (entries->dist (alist->entries (hash-table->alist table)))))

 (define (dist-sum dists)
   (dist-collapse (entries->dist (apply append (map dist->entries dists)))))

 (define (dist-scale d s)
   (entries->dist
    (map (lambda (v p) (list v (s* s p)))
         (dist-vals d)
         (dist-probs d))))

 (define (dist-mix dists probs)
   (let ([normalized-dists (map dist-scale dists probs)])
     (dist-sum normalized-dists)))

 (define (dist-mass dist)
   (sum (dist-probs dist)))

 (define null-dist (make-dist '() '()))

 (define (null-dist? d)
   (null? (dist-vals d)))

 )