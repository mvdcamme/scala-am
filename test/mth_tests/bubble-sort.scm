(define (bubble-sort vector <<?)  
  (define (bubble-swap vector idx1 idx2)
    (define keep (vector-ref vector idx1))
    (vector-set! vector idx1 (vector-ref vector idx2))
    (vector-set! vector idx2 keep)
    #t)
  (define (outer-loop unsorted-idx)
    (if (>= unsorted-idx 0)
        (if (letrec ((inner-loop (lambda (inner-idx has-changed?)
                                   (if (> inner-idx unsorted-idx)
                                       has-changed?
                                       (inner-loop (+ inner-idx 1)
                                                   (if (<<? (vector-ref vector (+ inner-idx 1))
                                                            (vector-ref vector inner-idx))
                                                       (bubble-swap vector inner-idx (+ inner-idx 1))
                                                       has-changed?))))))
              (inner-loop 0 #f))
            (outer-loop (- unsorted-idx 1)))))
  (outer-loop (- (vector-length vector) 2)))

(define (make-random-array length)
  (define v (make-vector length 0))
  (define (loop i)
    (if (< i  length)
        (begin (vector-set! v i (random 1000))
               (loop (+ i 1)))
        v))
  (loop 0))

(define random-vec (make-random-array 5))

(bubble-sort random-vec <)

random-vec