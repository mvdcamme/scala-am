(define false #f)
(define true #t)

(define (create-stack eq-fnct)
  (let ((content '()))
    (define (empty?)
      (null? content))
    (define (push element)
      (set! content (cons element content))
      #t)
    (define (pop)
      (if (null? content)
          #f
          (let ((temp (car content)))
            (set! content (cdr content))
            temp)))
    (define (top)
      (if (null? content)
          #f
          (car content)))
    (define (is-in element)
      (if (member element content)
          #t
          #f))
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty?)
        ((eq? m 'push) push)
        ((eq? m 'pop) pop)
        ((eq? m 'top) top)
        ((eq? m 'is-in) is-in)
        (else (error "unknown request
                     -- create-stack" m))))
    dispatch))

(define (create-set . same)
  (let ((content '())
        (same? (if (null? same) eq? (car same))))
    (define (empty?)
      (null? content))
    (define (is-in? item)
      (define (find-iter current)
        (cond ((null? current) false)
              ((same? item (car current)) true)
              (else (find-iter (cdr current)))))
      (find-iter content))
    (define (insert item)
      (if (not (is-in? item))
          (set! content (cons item content)))
      true)
    (define (delete item)
      (define (remove-iter current prev)
        (cond
          ((null? current) false)
          ((same item (car current))
           (if (null? prev)
               (set! content (cdr content))
               (set-cdr! prev (cdr current)))
           true)
          (else (remove-iter (cdr current) current))))
      (remove-iter content '()))
    (define (map a-function)
      (define (map-iter current result)
        (if (null? current)
            (reverse result)
            (map-iter (cdr current)
                      (cons (a-function (car current))
                            result))))
      (map-iter content '()))
    (define (foreach a-action)
      (define (foreach-iter current)
        (cond
          ((null? current) true)
          (else (a-action (car current))
                (foreach-iter (cdr current)))))
      (foreach-iter content)
      true)
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty?)
        ((eq? m 'is-in) is-in?)
        ((eq? m 'insert) insert)
        ((eq? m 'delete) delete)
        ((eq? m 'map) map)
        ((eq? m 'foreach) foreach)
        (else
          (error "unknown request
                 -- create-set" m))))
    dispatch))



(define (create-operator pre op) (list pre op))
(define (precondition operator) (car operator))
(define (operation operator) (cadr operator))


(define (depth-first-search start-state  operators
                            goal-reached? good-state? eq-state?
                            present-goal
                            open-trace-action
                            closed-trace-action)
  (let ((open (create-stack eq-state?))
        (closed (create-set eq-state?)))
    (define (expand state)
      (define (iter rest)
        (cond ((null? rest) false)
              (else
                (let ((operator (car rest)))
                  (if ((precondition operator) state)
                      (let ((new-state ((operation operator) state)))
                        (if (good-state? new-state)
                            (if (goal-reached? new-state)
                                (begin
                                  (if open-trace-action
                                      (open-trace-action new-state))
                                  (present-goal new-state)
                                  true)
                                (begin
                                  (if (and (not ((open 'is-in) new-state))
                                           (not ((closed 'is-in) new-state)))
                                      (begin
                                        ((open 'push) new-state)
                                        (if open-trace-action
                                            (open-trace-action new-state))))
                                  (iter (cdr rest))))
                            (iter (cdr rest))))
                      (iter (cdr rest)))))))
      (iter operators))
    (define (loop)
      (cond (((open 'empty?)) false)
            (else (let ((state ((open 'pop))))
                    ((closed 'insert) state)
                    (if closed-trace-action
                        (closed-trace-action state))
                    (let ((solution (expand state)))
                      (if solution
                          solution
                          (loop)))))))
    ((open 'push) start-state)
    (loop)))

(define operators
  (map (lambda(x)
         (create-operator (lambda(state)(not (vector-ref (car state) x)))
                          (lambda(state)(let ((newv (vector-copy (car state))))
                                          (vector-set! newv x (cadr state))
                                          (cons newv (cddr state))))))
       '(0 1 2 3 4 5 6 7 8)))

(define (good-state? state)
  (define (sum-or-f board . co)
    (cond ((null? co) 0)
          ((vector-ref board (car co))
           (let ((rest (apply sum-or-f (cons board (cdr co)))))
             (if rest (+ (vector-ref board (car co)) rest) rest)))
          (else #f)))
  (let ((board (car state))
        (sum #f))
    (define (check p1 p2 p3)
      (let ((s (sum-or-f board p1 p2 p3)))
        (or (not s)
            (if sum (= s sum)
                (begin (set! sum s)
                       #t)))))
    (and
      (check 0 1 2)
      (check 3 4 5)
      (check 6 7 8)
      (check 0 3 6)
      (check 1 4 7)
      (check 2 5 8))))

(define (goal-reached? state)
  (and (null? (cdr state)) (good-state? state)))

(define eq-state? equal?)

(define (present-goal state)
  (display (vector-ref (car state) 0))(display " ")
  (display (vector-ref (car state) 1))(display " ")
  (display (vector-ref (car state) 2))(display " ")
  (newline)
  (display (vector-ref (car state) 3))(display " ")
  (display (vector-ref (car state) 4))(display " ")
  (display (vector-ref (car state) 5))(display " ")
  (newline)
  (display (vector-ref (car state) 6))(display " ")
  (display (vector-ref (car state) 7))(display " ")
  (display (vector-ref (car state) 8))(display " ")
  (newline)(newline))

(define open-trace-action #f)

(define (closed-trace-action state)
  (present-goal state))
  
  
(define (magic-square numbers)
  (define start-state (cons (vector #f #f #f #f #f #f #f #f #f) numbers))
  (depth-first-search
    start-state
    operators
    goal-reached?
    good-state?
    eq-state?
    present-goal
    open-trace-action
    closed-trace-action))
  
(magic-square '(7 3 11 4 10 5 9 6 8))