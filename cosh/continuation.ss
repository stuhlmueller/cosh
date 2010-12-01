#!r6rs

;; continuations can be compared using "equal?"

(library

 (cosh continuation)

 (export make-continuation
         continuation:closure-id
         continuation:closure
         continuation:support
         continuation:scores
         continuation?
         continuations-equal?)

 (import (rnrs)
         (_srfi :1)
         (scheme-tools)
         (scheme-tools object-id))

 (define (make-continuation closure support scores)
   (list 'cont (object->id closure) support scores))
 
 (define continuation:closure-id second)
 
 (define (continuation:closure cont)
   (id->object (second cont)))
 
 (define continuation:support third)
 
 (define continuation:scores fourth)
 
 (define (continuation? obj)
   (tagged-list? obj 'cont))

 (define (continuations-equal? c1 c2)
   (eq? (continuation:closure-id c1)
        (continuation:closure-id c2)))

 )