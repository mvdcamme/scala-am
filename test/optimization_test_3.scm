(let* ((a #f)
       (f (lambda ()
            (if (= (random 2) 0)
                (set! a 'foo)
                (set! a 5))
            
            (define (loop n)
              (if (< n 0)
                  'done
                  (begin (display a)
                         (newline)
                         (loop (- n 1)))))
            
            (loop 10))))
  (f)
  (f))