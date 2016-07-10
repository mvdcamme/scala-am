(letrec ((x 10))
  (set! x 11)
  (letrec ((test (lambda (n)
                 x
                 (if (< n 0)
                     x
                     (test (- n 1))))))
    (test 100)))
