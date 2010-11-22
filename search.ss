#!r6rs

(import (rnrs)
        (scheme-tools)
        (transforms)
        (rhash))

(define (member? obj lst)
  (if (null? lst)
      #f
      (if (equal? (car lst) obj)
          #t
          (member? obj (cdr lst)))))

(define (dfs graph)
  (helper (list graph) '()))

(define (helper stack visited)
  (if (null? stack)
      visited
      (let ([currentNode (car stack)])
        (if (member? currentNode visited)
            (helper (cdr stack)
                    visited)
            (begin
              (pretty-print currentNode)
              (helper (append (node->children currentNode) (cdr stack))
                      (cons currentNode visited)))))))

(define node->children cdr)

(pretty-print (dfs '(1 (8 (2) (3) (4)) (9 (5) (6) (7)))))