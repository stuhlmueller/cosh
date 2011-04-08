#!r6rs

(library

 (cosh global)

 (export verbose
         verbose-pe)

 (import (rnrs)
         (scheme-tools))

 (define verbose (make-parameter #f))

 (define (verbose-pe . args)
   (when (verbose)
         (apply pe args)))

 )