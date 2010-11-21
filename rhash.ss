#!r6rs

;; hasher that can deal with recursive references

;; - throws out procedures; assumes that the object
;;   given fully mirrors procedure information in a
;;   more accessible way

(library

 (rhash)

 (export (rename (make-rhash-table make-hash-table))
         alist->hash-table
         hash-table->alist
         hash-table-ref
         hash-table-ref/default
         hash-table-set!
         hash-table-delete!
         hash-table-exists?
         hash-table-update!
         hash-table-update!/default
         hash-table-size
         hash-table-keys
         hash-table-values
         hash-table-walk
         hash-table-fold
         hash-table-copy
         hash-table-merge!)
 
 (import (rnrs)
         (scheme-tools)
         (transforms)
         (except (_srfi :69) string-hash string-ci-hash))

 (define (finitize obj)
   (define seen '())
   (define sym (symbol-maker 's))
   (define (fin obj)
     (let ([s (assq obj seen)])
       (if s
           (cdr s)
           (cond [(procedure? obj) 'proc]
                 [(pair? obj)
                  (begin (set! seen (cons (cons obj (sym)) seen))
                         (cons (fin (car obj))
                               (fin (cdr obj))))]
                 [(vector? obj)
                  (begin (set! seen (cons (cons obj (sym)) seen))
                         (map fin (vector->list obj)))]
                 [else obj]))))
   (fin obj))

 (define (rhash obj bound)
   (hash (finitize obj) bound))

 (define (requal? obj1 obj2)
   (equal? (finitize obj1)
           (finitize obj2)))

 (define (make-rhash-table)
   (make-hash-table requal? rhash))

 (define (test)
   (define test-obj (vector (lambda (x) x) 2 3))
   (define ht (make-hash-table))   
   (vector-set! test-obj 2 test-obj)
   (hash-table-set! ht test-obj 1)
   (display (hash-table-ref ht test-obj)))

 )