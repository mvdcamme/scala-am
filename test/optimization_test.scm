(letrec ((test (lambda (a b)
                 (if (< a 1)
                   0
                   (test (- a 1) (* (+ 1 2) (- 11 1)))))))
  (test 5 0))
