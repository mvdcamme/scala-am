(let ((a (amb 1 2 3)))
  (if (= (modulo a 2) 0)
      a
      (amb )))