;(define (f)
;  'error)
;(define (g)
;  'ok)
;
;(define (loop n x)
;  (if (<= n 0)
;      x
;      (loop (- n 1) (+ x 1))))

(let* ((a (random 2000000))
       (b (random 2000000)))
      ; (c (random 99))
      ; (d (+ a b))
      ; (h f))
  ;(loop 5 (random 99))
  (display a)
  (display b)
  (if (> a 5)
      (if (> b 5)
          (error "error1")
          (error "error2"))
      'ok2))