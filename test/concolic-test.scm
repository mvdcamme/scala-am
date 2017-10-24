(define (loop n)
  (if (< n 0)
      99
      (loop (- n 1))))

(let ((a (random 10)))
  (loop a))