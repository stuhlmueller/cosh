#!r6rs

(library

 (cosh call)

 (export make-call
         call-with-cont
         call
         calls-delimited-equal?
         calls-equal?
         call:args
         call:cont
         call:proc
         call:delimited-id
         call:id)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (scheme-tools deepcopy)
         (scheme-tools object-id))

 (define (make-call proc cont . args)
   (list 'call
         (object->id (list proc cont args))
         (object->id (list proc args))))

 (define call:id second)
 
 (define call:delimited-id third)

 (define (call:proc c)
   (first (id->object (call:id c))))

 (define (call:cont c)
   (second (id->object (call:id c))))

 (define (call:args c)
   (third (id->object (call:id c))))

 (define (calls-equal? c1 c2)
   (eq? (call:id c1)
        (call:id c2)))

 (define (calls-delimited-equal? c1 c2)
   (eq? (call:delimited-id c1)
        (call:delimited-id c2)))

 (define (call c)
   (let ([c1 (deepcopy c)])
     (apply (vector-ref (call:proc c1) 0)
            (pair (call:proc c1) (pair (call:cont c1) (call:args c1))))))

 ;; cont is a continuation closure vector
 (define (call-with-cont c cont)
   (let ([c1 (deepcopy c)]
         [cont1 (deepcopy cont)])
     (apply (vector-ref (call:proc c1) 0)
            (pair (call:proc c1) (pair cont1 (call:args c1))))))

 )