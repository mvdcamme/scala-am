(letrec ((fact (lambda (n)
                 (if (= n 0)
                   1
                   (* n (fact (- n 1))))))
         (id (lambda (x) x)))
  (fact 8)
  (let* ((y (id #t))
         (z (id #f)))
    y))
