#!r6rs

;; continuations can be compared using "equal?"

(library

 (cosh continuation)

 (export make-continuation
         continuation:id
         continuation:closure
         continuation:support
         continuation:scores
         continuation?
         continuations-equal?
         call-continuation)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (scheme-tools deepcopy)
         (scheme-tools object-id))

 (define (make-continuation closure support scores)
   (list 'cont (object->id (list closure support scores)) support scores))
 
 (define continuation:id second)
 
 (define (continuation:closure cont)
   (first (id->object (continuation:id cont))))
 
 (define continuation:support third)
 
 (define continuation:scores fourth)
 
 (define (continuation? obj)
   (tagged-list? obj 'cont))

 (define (continuations-equal? c1 c2)
   (eq? (continuation:id c1)
        (continuation:id c2)))

 (define (call-continuation cont value)
   (let ([clos (deepcopy (continuation:closure cont))])
     ((vector-ref clos 0) clos value)))

 )