; expected result: 40320
(letrec ((fact (lambda (n)
                 (if (= n 0)
                   2
                   (* n (fact (- n 1)))))))
  (fact 8))
