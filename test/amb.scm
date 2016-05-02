(let ((a (amb 1 2)))
  (if (= a 2)
      a
      (amb)))