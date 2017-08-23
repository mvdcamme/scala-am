(let* ((a (random 10))
       (b (random 10))
       (c (random 10))
       (d (+ a b)))
  (if (< d 0)
      (if (< a 0)
          'error
          'ok)
      'ok))