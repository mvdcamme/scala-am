(define (make-point1 x y)
  (define (get-x)
    x)
  (define (dispatch msg)
    (cond ((eq? msg 'get-x) get-x)
          (else #f)))
  dispatch)

(define (make-point2 x y)
  (define (get-x)
    x)
  (define (dispatch msg)
    (cond ((eq? msg 'get-x) get-x)
          (else #f)))
  dispatch)

(define p1 (make-point1 1 2))
(define p2 (make-point2 3 4))

(define (f p)
  ((p 'get-x)))

(f p2)
(f p1)