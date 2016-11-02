(define (id y)
  y)

(define (loop n x)
  (cond ((< n 1) x)
        ((= (modulo n 2) 0)
         (let ((a (cons 1 2)))
           (loop (- n 1) (id a))))
        (else
         (let ((b (cons 3 4)))
           (loop (- n 1) (id b))))))

(loop 100 'done)