#!r6rs

(library

 (cosh global)

 (export merge-continuations
         verbose
         verbose-pe)

 (import (rnrs)
         (scheme-tools))

 (define merge-continuations (make-parameter #f))

 (define verbose (make-parameter #f))

 (define (verbose-pe . args)
   (when (verbose)
         (apply pe args)))

 )