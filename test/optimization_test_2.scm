(let ((x (random 10))
      (y (random 10))
      (z (random 10)))
  (define (loop1 n)
    (if (< n 0)
        "done"
        (loop1 (- n x y z 1))))
  (loop1 10000)
  (set! y (random 10))
  (set! z (random 10))
  (define (loop2 n)
    (if (< n 0)
        "done"
        (loop2 (- n x y z 1))))
  (loop2 10000)
  (set! z (random 10))
  (define (loop3 n)
    (if (< n 0)
        "done"
        (loop3 (- n x y z 1))))
  (loop3 10000))
