(letrec ((x 10)
         (test (lambda (a b ignored)
                 (if (< a 1)
                   b
                   (test (- a 1) (+ 1 x) (set! x 2.0))))))
  (test 5 0 0))
