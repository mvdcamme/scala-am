(define (f)
  'error)
(define (g)
  'ok)

(define (loop n x)
  (if (<= n 0)
      x
      (loop (- n 1) (+ x 1))))

(let* ((a (random 99))
       (b (random 99))
       (c (random 99))
       (d (+ a b))
       (h f))
  (display (loop 5 (random 99)))
  (if (< d 0)
      (if (< a 0)
          'error
          'ok1)
      'ok2))