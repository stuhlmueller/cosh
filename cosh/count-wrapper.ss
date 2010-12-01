#!r6rs

;; Shortcutting enumeration based on continuation hashing

(import (rnrs)
        (scheme-tools)
        (transforms)
        (scheme-tools hash))

(define (transform expr reserved-words)
  (cc-transform
   (cps-transform
    (assignment-transform
     (letrec-to-set
      expr))
    reserved-words)
   reserved-words))

(define (wrap e)
  `(begin
     (define search-stack '())
     (define history '())
     (define future '())
     (define (raw-flip)
       (if (null? future)
           (begin
             (set! search-stack
                   (append (list (cons #f history))
                           search-stack))
             (set! history (cons #t history))
             #t)
           (begin
             (let ([v (car future)])
               (set! future (cdr future))
               (set! history (cons v history))
               v))))
     (define count-table (make-hash-table))
     (define value-table (make-hash-table))
     (define (ht-inc! ht k)
       (let ([count (hash-table-ref/default ht k 0)])
         (hash-table-set! ht k (+ count 1))))
     (define (enumerate proc)
       (set! count-table (make-hash-table))
       (set! search-stack '())
       (set! history '())
       (set! future '())
       (let ([v1 (proc)]
             [h1 history])
         (set! history '())
         (let loop ([vals (list (list v1 (reverse h1)))])
           (if (null? search-stack)
               vals
               (begin
                 (set! history '())
                 (set! future (reverse (car search-stack)))
                 (set! search-stack (cdr search-stack))
                 (let ([v (proc)])
                   (loop (cons (list v (reverse history)) vals))))))))
     (define flip
       (vector
        (lambda (self k)
          (let ([flip-val (raw-flip)])
            (ht-inc! count-table (cons flip-val k))
            ((vector-ref k 0)
             k
             flip-val)))))
     (define (my-and a b)
       (and a b))
     (define (my-or a b)
       (or a b))
     (define print-future pretty-print)
     (pretty-print (enumerate (lambda () (begin ,@(cdr e)))))
     (pretty-print (map cdr (hash-table->alist count-table)))))

(define prog
  '%(expr)s)

(define te
  (transform prog
             '(flip)))

(pretty-print (wrap te))