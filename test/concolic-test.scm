(define a (vector 0 99))
(if (= (random 1) 1)
    (vector-set! a 0 10)
    (vector-set! a 0 11))
(vector-ref a 0)
(error "wefwe")