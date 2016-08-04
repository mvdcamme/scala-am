(letrec ((x 10)
         (y 100))
  (set! x 11)
  (letrec ((test (lambda (n)
                   x
                   y
                   (if (< n 0)
                       x
                       (test (- n 1))))))
    (test 100)))
