#!r6rs

(library

 (cosh-test utils)

 (export write-to-string)

 (import (rnrs)
         (rnrs io ports))

 (define (write-to-string val)
  (let-values (((string-port extractor) (open-string-output-port)))
    (write val string-port)
    (extractor)))

 )