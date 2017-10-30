(define (foo i b)
  (if (> i 200000)
      (if (not b)
          (error "b"))))

(foo (random 10) (if (= (random 10) 0) #f #t))