(define (require pred)
  (if (not pred)
      (amb)
      'stop))

(define (an-integer-between low high)
  (if (> low high)
      (amb)
      (amb low (an-integer-between (+ low 1) high))))

(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
         (j (an-integer-between (+ i 1) high))
         (k (an-integer-between (+ j 1) high)))
    (require (= (+ (* i i) (* j j)) (* k k)))
    (display (+ (* i 10000) (* j 100) k))
    (amb)))

(a-pythagorean-triple-between 1 25)