(let ((sq (lambda (x) (* x x))))
  (sq (sq (sq (sq (sq (sq 2))))))
  (sq 3))
