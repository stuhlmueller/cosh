#!r6rs

(library

 (cosh watcher)

 (export get-watcher)

 (import (rnrs)
         (scheme-tools hash))

 (define (get-watcher)
   (let ([time-tables (make-hash-table)])
     (lambda (node . maybe-t)
       (let ([t (if (null? maybe-t) 1 (car (maybe-t)))])
         (let ([time-table (hash-table-ref time-tables
                                           t
                                           (lambda () (let ([table (make-hash-table)])
                                                   (hash-table-set! time-tables t table)
                                                   table)))])
           (let ([seen (hash-table-ref/default time-table
                                               node
                                               #f)])
             (when (eq? seen #f)
                   (hash-table-set! time-table node #t))
             seen))))))

 )