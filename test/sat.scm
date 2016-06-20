(define (phi x1 x2 x3 x4)
  (if (let ((a x1))
        (if a
            a
            (let ((b (not x2)))
              (if b
                  b
                  (not x3)))))
      (if (let ((a (not x2)))
            (if a
                a
                (not x3)))
          (let ((a x4))
            (if a
                a
                x2))
          #f)
      #f))
(define (try f)
  (let ((a (f #t)))
    (if a
        a
        (f #f))))
(define (sat-solve-4 p)
  (try (lambda (n1)
         (try (lambda (n2)
                (try (lambda (n3)
                       (try (lambda (n4)
                              (p n1 n2 n3 n4))))))))))

(sat-solve-4 phi)
