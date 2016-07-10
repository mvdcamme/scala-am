(letrec ((inc (lambda (x) (+ x 1))))
  (inc (inc (inc (inc (inc (inc (inc (inc 2)))))))))
