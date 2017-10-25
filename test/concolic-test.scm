(define (f x)
  (+ x 1))

(let ((a (random 10))
      (b (random 10)))
  (if (< a (f 4))
      (if (< b 5)
          'ok
          (error "a"))      
      'ok))