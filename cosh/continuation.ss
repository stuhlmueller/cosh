#!r6rs

(library

 (cosh continuation)

 (export make-continuation
         continuation:closure-id
         continuation:closure
         continuation:support
         continuation:scores
         continuation?)

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

 )