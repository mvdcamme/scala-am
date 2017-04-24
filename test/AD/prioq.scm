(define (make-item priority element)
  (cons priority element))

(define true #t)
(define false #f)

(define (get-priority item) (car item))

(define (get-element item) (cdr item))

(define (create-priority-queue)
  (let ((front (cons 'boe '())))
    (define (content) (cdr front))
    (define (insert-after! cell item)
      (let ((new-cell (cons item '())))
        (set-cdr! new-cell (cdr cell))
        (set-cdr! cell new-cell)))
    (define (find-prev-cell priority)
      (define (find-iter rest prev)
        (cond
          ((null? rest) prev)
          ((> (get-priority (car rest)) priority)
           (find-iter (cdr rest) rest))
          (else prev)))
      (find-iter (content) front))
    (define (empty?)
      (null? (content)))
    (define (enqueue priority element)
      (insert-after! (find-prev-cell priority)
                     (make-item priority element))
      true)
    (define (dequeue)
      (if (null? (content))
          false
          (let ((temp (car (content))))
            (set-cdr! front (cdr (content)))
            (get-element temp))))
    (define (serve)
      (if (null? (content))
          false
          (get-element (car (content)))))
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty?)
        ((eq? m 'enqueue) enqueue)
        ((eq? m 'dequeue) dequeue)
        ((eq? m 'serve) serve)
        (else
          (error "unknown request
                 -- create-priority-queue" m))))
    dispatch))

(define c (create-priority-queue))
((c 'enqueue) 3 "g")
((c 'enqueue) 1 "f")
((c 'enqueue) 5 "x")