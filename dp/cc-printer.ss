#!r6rs

(import (rnrs)
        (scheme-tools)
        (transforms))

(define (transform expr reserved-words)
  (let* ([a (letrec-to-set expr)]
         ;;         [_ (display "1 ...")]
         [b (assignment-transform a)]
         ;;         [_ (display "2 ...")]
         [c (cps-transform b reserved-words)]
         [d (redex-transform c)]
         ;; [_ (pretty-print d)]
         [e (cc-transform d reserved-words)]
         ;;         [_ (display "4 ...")]
         )
    e))

(pretty-print
 (transform
  '%(expr)s
  '(v-apply flip sample-integer)))