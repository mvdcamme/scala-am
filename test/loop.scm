(define (loop n)
  (if (<= n 0)
      'ok
      (loop (- n 1))))
(loop (random 10))
(error 10)