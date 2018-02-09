(define (make-something)
  (define qqq 10)
  (define (set-qqq! q)
    (set! qqq q))
  (define (f x)
    (+ x qqq))
  (define (dispatch msg)
    (cond ((eq? msg 'f) f)
          ((eq? msg 'set-qqq!) set-qqq!)
          (else (error msg))))
  dispatch)

(let ((a (random 10))
      (b (random 10))
      (obj (make-something)))
  ((obj 'set-qqq!) 100)
  (display a) (newline)
  (display b) (newline)
  (if (< a ((obj 'f) 4))
      (if (< b 5)
          (error 'a)
          'ok)
      (error 'a)))