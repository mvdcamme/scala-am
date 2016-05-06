(define (require pred)
  (if (not pred)
      (amb)
      'stop))

(define (an-integer-between loww highh)
  (if (> loww highh)
      (amb)
      (amb loww (an-integer-between (+ loww 1) highh))))

(define (a-pythagorean-triple-between low high)
  (let* ((i (an-integer-between low high))
         (j (an-integer-between (+ i 1) high))
         (k (an-integer-between (+ j 1) high)))
    (require (= (+ (* i i) (* j j)) (* k k)))
    (display (+ (* i 10000) (* j 100) k))
    (amb)))

(a-pythagorean-triple-between 1 25)