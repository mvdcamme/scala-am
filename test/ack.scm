; expected result: 4
(letrec ((ack (lambda (m n)
                (if (= m 0)
                    (+ n 1)
                    (if (= n 0)
                        (ack (- m 1) 1)
                        (ack (- m 1) (ack m (- n 1))))))))
  (ack 1 2))
