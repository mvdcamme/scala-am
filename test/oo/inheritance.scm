;;; From: https://openresearch-repository.anu.edu.au/bitstream/1885/40783/3/TR-CS-94-02.pdf

(define (make-a)
  (let ((private-a-member 0)
        (public-a-member 0))
    (define (method-1)
      #f)
    (define (method-2)
      #f)
    (define (dispatch msg)
      (cond ((eq? msg 'method-1) method-1)
            ((eq? msg 'method-2) method-2)
            (else (error "Message not recognized"))))
    dispatch))

(define (make-b)
  (let ((private-b-member 0)
        (public-b-member 0))
    (define (method-3)
      #f)
    (define (method-4)
      #f)
    (define (dispatch msg)
      (cond ((eq? msg 'method-3) method-3)
            ((eq? msg 'method-4) method-4)
            (else (error "Message not recognized"))))
    dispatch))

(define (make-c)
  (let ((private-a-member 0)
        (public-a-member 0)
        (private-b-member 0)
        (public-b-member 0))
    (define (method-1)
      #f)
    (define (method-2)
      #f)
    (define (method-3)
      #f)
    (define (method-4)
      #f)
    (define (dispatch msg)
      (cond ((eq? msg 'method-1) method-1)
            ((eq? msg 'method-2) method-2)
            ((eq? msg 'method-3) method-3)
            ((eq? msg 'method-4) method-4)
            (else (error "Message not recognized"))))
    dispatch))

(define b (make-b))
(define c (make-c))
((b 'method-3))
((c 'method-4))