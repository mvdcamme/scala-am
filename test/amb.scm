(let ((a (amb 5 1 5 2 3)))
  (if (= a 2)
      a
      (amb)))