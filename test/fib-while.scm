(let ((count 9)
      (a 1)
      (b 0)
      (temp 0))
  (letrec ((f (lambda (n) n)))
    (while (> (f count) 0)
           (set! temp a)
           (set! a (+ a b))
           (set! b temp)
           (set! count (- count 1)))
  a))
