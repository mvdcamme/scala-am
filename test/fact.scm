; expected result: 40320
(letrec ((fact (lambda (x)
                 (define (loop n acc)
                   (if (= n 0)
                       acc
                       (loop (- n 1) (* n acc))))
                 (loop x 1))))
  (fact 8))
