(begin
  (define recurse
    (lambda (n)
      (if (= n 0)
          (flip)
          ((if (= (mod n 2) 0) my-and my-or)
           (recurse (- n 1)) (recurse (- n 1))))))
  (recurse 3))