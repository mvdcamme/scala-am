(begin (define (loop n)
         (define g '()) ;no forward referencing
         (define (f)
           (if (= (random 2) 0)
               (begin (display "f 0") (newline))
               (begin (display "f 1") (newline)))
           (g))
         (define (h)
           (if (= (random 2) 0)
               (begin (display "h 0") (newline))
               (begin (display "h 1") (newline)))
           'h)
         (define (gg)
           (display "in g") (newline)
           (if (= (random 2) 0)
               (begin (display "g 0") (newline))
               (begin (display "g 1") (newline)))
           (h))
         (set! g gg)
         (if (> n 0)
             (begin (f)
                    (loop (- n 1)))))
       
       (loop 30))