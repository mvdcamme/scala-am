(define (foo b k0 k1 k2 d0 d1 d2 d3 state i)
  (if (> i 200000)
      (begin (if (= b 1)
                 (if (= k0 k1)
                     (if (not (= d1 d3))
                         (set! d0 (+ d1 d3))
                         (set! d1 (+ d0 d3)))))
             (set! state k0))
      (set! state k1)))


(foo (random 10) (random 10) (random 10) (random 10) (random 10) (random 10) (random 10) (random 10) (random 10) (random 10))