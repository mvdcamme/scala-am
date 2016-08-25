(letrec ((x 1)
         (y (random 10)))
  (define (test n)
                 (if (< n 0)
                     "done"
                     (test (- n x y))))
  (test 1000))
