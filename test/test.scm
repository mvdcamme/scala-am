(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (map f (cdr lst)))))
(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f acc (car lst)) (cdr lst))))

(define (int-fun1 i) (+ i 2))
(define (int-fun2 i) (= (modulo i 2) 0))
(define (bool-fun1 b) (not b))
(define (bool-fun2 b1 b2) (and b1 b2))

(define list1 '(1 2 3 4 5 6 7 8 9 10))
(define list2 (map int-fun1 list1))
(define list3 (map int-fun2 list2))
(define list4 (map bool-fun1 list3))
(foldl bool-fun2 #t list4)