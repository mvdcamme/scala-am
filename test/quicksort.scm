(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (split-by l p k)
  (letrec ((loop (lambda (low high l)
                   (cond ((null? l)
                          (k low high))
                         ((p (car l))
                          (loop low (cons (car l) high) (cdr l)))
                         (else
                          (loop (cons (car l) low) high (cdr l)))))))
    (loop '() '() l)))

(define (quicksort l gt?)
  (if (null? l)
      '()
      (split-by (cdr l) 
                (lambda (x) (gt? x (car l)))
                (lambda (low high)
                  (append (quicksort low gt?)
                          (append (cons (car l) '())
                                  (quicksort high gt?)))))))

(define (larger-than a b)
  (> a b))

(define (smaller-than a b)
  (< a b))

(define list '(1 7 2 6))
(quicksort list larger-than)
(quicksort list smaller-than)