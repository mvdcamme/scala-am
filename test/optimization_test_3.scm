(let* ((a #f)
       (f (lambda ()
            (if (= 1 0)
                (set! a 'foo)
                (set! a 10))
            
            (define (loop n)
              (if (< n 0)
                  'done
                  (begin (display a)
                         (newline)
                         (loop (- n 1)))))
            
            (loop 10))))
  (f)
  (f))