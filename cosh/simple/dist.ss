#!r6rs

;; Distribution data structure that supports symbolic math

(library

 (cosh simple dist)

 (export dist?
         dist->entries
         dist-collapse
         dist-mix
         dist-elements
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
         (scheme-tools srfi-compat :1)
         (srfi :41))

 (define-record-type dist
   (fields elements)
   (protocol
    (lambda (p)
      (lambda (e)
        (assert (stream? e))
        (p e)))))
 
 (define (dist-vals d)
   (stream-map first (dist-elements d)))

 (define (dist-probs d)
   (stream-map rest (dist-elements d)))

  (define (stream-assoc v s)
   (if (stream-null? s)
       #f
       (if (equal? (car (stream-car s)) v)
           (stream-car s)
           (stream-assoc v (stream-cdr s)))))
 
 (define (dist-prob d v)
   (let ([dp (stream-assoc v (dist-elements d))])
     (if dp (rest dp) 0.0)))

 (define entry->val first)

 (define entry->prob second)

 (define (pair->list x)
   (list (car x) (cdr x)))
 
 (define (dist->entries d)
   (stream-map pair->list (dist-elements d)))
 
 (define (entries->dist entries)
   (make-dist (stream-map cons
                          (stream-map first entries)
                          (stream-map second entries))))

 (define (pretty-print-dist d)
   (stream-for-each (lambda (v p)
                      (pe v ": " p "\n"))
                    (dist-vals d)
                    (dist-probs d)))

 (define (singleton-dist val)
   (make-dist (list->stream (list (cons val 1.0)))))

 (define (distify val)
   (if (dist? val)
       val
       (singleton-dist val)))

 (define (dist-product dists f)
   (make-dist
    (stream-map (lambda (entries)
                  (pair (f (map entry->val entries))
                        (apply s* (map entry->prob entries))))
                (stream-all-combinations (map dist->entries dists)))))

 ;; (define (dist-collapse d)
 ;;   (let ([table (make-equal-hash-table)])
 ;;     (for-each (lambda (v p)
 ;;                 (let ([p0 (hash-table-ref/default table v #f)])                
 ;;                   (hash-table-set! table
 ;;                                    v
 ;;                                    (if p0 (s+ p0 p) p))))
 ;;               (dist-vals d)
 ;;               (dist-probs d))
 ;;     (make-dist (hash-table->alist table))))

 ;; (define (dist-sum dists)
 ;;   (dist-collapse (entries->dist (apply append (map dist->entries dists)))))

 (define (dist-sum dists)
   (entries->dist (apply stream-append (map dist->entries dists))))

 (define (dist-scale d s)
   (make-dist
    (stream-map (lambda (v p) (pair v (s* s p)))
                (dist-vals d)
                (dist-probs d))))

 (define (dist-mix dists probs)
   (let ([normalized-dists (map dist-scale dists probs)])
     (dist-sum normalized-dists)))

 )