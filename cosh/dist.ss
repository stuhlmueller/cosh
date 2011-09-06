#!r6rs

;; Distribution data structure that supports symbolic math

(library

 (cosh dist)

 (export alist->dist
         dist?
         dist->alist
         dist->entries
         dist-collapse
         dist-mix
         dist-prob
         dist-probs
         dist-product
         dist-scale
         dist-sum
         dist-vals
         distify
         entries->dist
         entry->prob
         entry->val
         make-dist
         pretty-print-dist
         singleton-dist)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools hash)
         (scheme-tools math symbolic)
         (scheme-tools srfi-compat :1))

 (define-record-type dist
   (fields vals probs))

 (define (dist-prob d v)
   (let ([dp (assoc v (dist->alist d))])
     (if dp (rest dp) 0.0)))

 (define entry->val first)

 (define entry->prob second)

 (define (dist->entries d)
   (zip (dist-vals d)
        (dist-probs d)))    

 (define (entries->dist entries)
   (make-dist (map first entries)
              (map second entries)))

 (define (dist->alist d)
   (map pair
        (dist-vals d)
        (dist-probs d)))

 (define (alist->dist alist)
   (make-dist (map first alist)
              (map rest alist)))

 (define (pretty-print-dist d)
   (for-each (lambda (v p)
               (pe v ": " p "\n"))
             (dist-vals d)
             (dist-probs d)))

 (define (singleton-dist val)
   (make-dist (list val)
              (list 1.0)))

 (define (distify val)
   (if (dist? val)
       val
       (singleton-dist val)))

 (define (dist-product dists f)
   (alist->dist
    (map (lambda (entries)
           (pair (f (map entry->val entries))
                 (apply s* (map entry->prob entries))))
         (all-combinations (map dist->entries dists)))))

 (define (dist-collapse d)
   (let ([table (make-equal-hash-table)])
     (for-each (lambda (v p)
                 (let ([p0 (hash-table-ref/default table v #f)])                
                   (hash-table-set! table
                                    v
                                    (if p0 (s+ p0 p) p))))
               (dist-vals d)
               (dist-probs d))
     (alist->dist (hash-table->alist table))))

 (define (dist-sum dists)
   (dist-collapse (entries->dist (apply append (map dist->entries dists)))))

 (define (dist-scale d s)
   (alist->dist
    (map (lambda (v p) (pair v (s* s p)))
         (dist-vals d)
         (dist-probs d))))

 (define (dist-mix dists probs)
   (let ([normalized-dists (map dist-scale dists probs)])
     (dist-sum normalized-dists)))

 )