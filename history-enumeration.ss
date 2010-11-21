#!r6rs

;; Full enumeration based on history of random choices

(import (rnrs)
        (scheme-tools))

(define search-stack '())
(define history '())
(define future '())

(define (flip)
  (if (null? future)
      (begin
        (set! search-stack
              (append (list (cons #f history))
                      search-stack))
        (set! history (cons #t history))
        #t)
      (begin
        (let ([v (car future)])
          (set! future (cdr future))
          (set! history (cons v history))
          v))))

(define (enumerate proc)
  (set! search-stack '())
  (set! history '())
  (set! future '())
  (let ([v1 (proc)]
        [h1 history])
    (set! history '())
    (let loop ([vals (list (cons v1 (reverse h1)))])
      (if (null? search-stack)
          vals
          (begin
            (set! history '())
            (set! future (reverse (car search-stack)))
            (set! search-stack (cdr search-stack))
            (let ([v (proc)])
              (loop (cons (cons v (reverse history)) vals))))))))

(define (prog)
  (if (flip)
      (if (flip)
          (if (flip) 'A 'D)
          'B)
      (if (flip)
          'C
          (if (flip) 'E 'F))))

(pretty-print (enumerate prog))