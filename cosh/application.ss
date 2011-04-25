#!r6rs

(library

 (cosh application)

 (export make-application
         application?
         call-application-with-cont
         call-application-cont
         call-application
         applications-delimited-equal?
         applications-equal?
         application:args
         application:cont
         application:proc
         application:delimited-id
         application:id)

 (import (rnrs)
         (cosh global)
         (scheme-tools)
         (scheme-tools srfi-compat :1)
         (scheme-tools deepcopy)
         (scheme-tools object-id))

 (define (make-application proc cont . args)
   (let ([app-id (if (merge-continuations) 'app (gensym))])
     (when (not (vector? proc))
           (repl proc cont args))
     (list 'application
           (object->id (list app-id proc cont args))
           (object->id (list proc args)))))

 (define (application? obj)
   (tagged-list? obj 'application))

 (define application:id second)
 
 (define application:delimited-id third)

 (define (application:proc c)
   (second (id->object (application:id c))))

 (define (application:cont c)
   (third (id->object (application:id c))))

 (define (application:args c)
   (fourth (id->object (application:id c))))

 (define (applications-equal? c1 c2)
   (eq? (application:id c1)
        (application:id c2)))

 (define (applications-delimited-equal? c1 c2)
   (eq? (application:delimited-id c1)
        (application:delimited-id c2)))

 (define (call-application c)
   (let ([proc1 (deepcopy (application:proc c))]
         [cont1 (deepcopy (application:cont c))]
         [args1 (deepcopy (application:args c))])
     (apply (vector-ref proc1 0)
            (pair proc1 (pair cont1 args1)))))

 ;; call the cont in application
 (define (call-application-cont c val)
   (let ([cont1 (deepcopy (application:cont c))]
         [val1 (deepcopy val)])
     ((vector-ref cont1 0) cont1 val1)))

 ;; cont is a continuation closure vector
 (define (call-application-with-cont c cont)
   (let ([proc1 (deepcopy (application:proc c))]
         [cont1 (deepcopy cont)]
         [args1 (deepcopy (application:args c))])
     ;; (pretty-print proc1)
     (apply (vector-ref proc1 0)
            (pair proc1 (pair cont1 args1)))))

 )