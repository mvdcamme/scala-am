(let ((a #f))
  (define (f b)
    (if b
        (set! a (cons 1 2))
        (set! a (cons 3 4))))
  (f #t)
  (f #f)
  (display a)
  (newline)
  a)
  
