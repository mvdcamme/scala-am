;(define (f)
;  'error)
;(define (g)
;  'ok)
;
;(define (loop n x)
;  (if (<= n 0)
;      x
;      (loop (- n 1) (+ x 1))))

(let* ((a (random 99))
       (b (random 99)))
      ; (c (random 99))
      ; (d (+ a b))
      ; (h f))
  ;(loop 5 (random 99))
  (display a)
  (display b)
  (if (< a 0)
      (if (< b 10)
          'ok1
          (error "error2"))
      'ok2))