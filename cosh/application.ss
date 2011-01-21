#!r6rs

(library

 (cosh application)

 (export make-app
         app?
         call-app-with-cont
         call-app-cont
         call-app
         apps-delimited-equal?
         apps-equal?
         app:args
         app:cont
         app:proc
         app:delimited-id
         app:id)

 (import (rnrs)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (scheme-tools deepcopy)
         (scheme-tools object-id))

 (define (make-app proc cont . args)
   (list 'app
         (object->id (list proc cont args))
         (object->id (list proc args))))

 (define (app? obj)
   (tagged-list? obj 'app))

 (define app:id second)
 
 (define app:delimited-id third)

 (define (app:proc c)
   (first (id->object (app:id c))))

 (define (app:cont c)
   (second (id->object (app:id c))))

 (define (app:args c)
   (third (id->object (app:id c))))

 (define (apps-equal? c1 c2)
   (eq? (app:id c1)
        (app:id c2)))

 (define (apps-delimited-equal? c1 c2)
   (eq? (app:delimited-id c1)
        (app:delimited-id c2)))

 (define (call-app c)
   (let ([c1 (deepcopy c)])
     (apply (vector-ref (app:proc c1) 0)
            (pair (app:proc c1) (pair (app:cont c1) (app:args c1))))))

 ;; call the cont in app
 (define (call-app-cont c val)
   (let ([cont1 (deepcopy (app:cont c))]
         [val1 (deepcopy val)])
     ((vector-ref cont1 0) cont1 val1)))

 ;; cont is a continuation closure vector
 (define (call-app-with-cont c cont)
   (let ([c1 (deepcopy c)]
         [cont1 (deepcopy cont)])
     (apply (vector-ref (app:proc c1) 0)
            (pair (app:proc c1) (pair cont1 (app:args c1))))))

 )