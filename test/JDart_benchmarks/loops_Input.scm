(define (foo i)
  (define (loop j)
    (if (< j 4)
        (begin (if (> (+ i j) 0)
                   (begin (display (+ i j)) (display " is > 0") (newline))
                   (begin (display (+ i j)) (display " is <= 0") (newline)))
               (loop (+ j 1)))))
  (loop 1))


(foo (random 10))