#!r6rs

(library

 (cosh abort)

 (export make-abort
         abort?)

 (import (rnrs)
         (scheme-tools))

 (define (make-abort)
   (list 'abort))

 (define (abort? obj)
   (tagged-list? obj 'abort))

 )