;;; DIVITER -- Gabriel Benchmark which divides by 2 using lists of n ()'s.
 
(let*
 ((create-n (lambda (n)
              (letrec ((do-loop1 (lambda (n a)
                                   (if (= n 0)
                                       a
                                       (do-loop1 (- n 1) (cons '() a))))))
                (do-loop1 n '()))))
  (*ll* (create-n 200))
  (iterative-div2 (lambda (l)
                    (letrec ((do-loop2 (lambda (l a)
                                         (if (null? l)
                                             a
                                             (do-loop2 (cddr l) (cons (car l) a))))))
                      (do-loop2 l '())))))
  (iterative-div2 *ll*))
