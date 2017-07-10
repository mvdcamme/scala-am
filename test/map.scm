(letrec ((map (lambda (f lst)
                (if (null? lst)
                    '()
                    (cons (f (car lst)) (map f (cdr lst))))))
         (g (lambda (x) (+ x 10)))
         (h (lambda (y)  (* y 10))))
  (define l '(1))
  (map g l)
  (map h l))