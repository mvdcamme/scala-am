(define (make-list n value)
    (if (zero? n) 
        '()
        (cons value (make-list (- n 1) value))))

(define (append a b)
  (if (null? a)
      b
      (cons (car a) (append (cdr a) b))))

(define (remove-all s lst) 
    (if (null? lst)
        '()
        (if (eq? s (car lst))
            (remove-all s (cdr lst))
            (cons (car lst) (remove-all s (cdr lst))))))

(define (initial-state n)
    (make-list n 1))

(define (move-check? src-peg dst-peg state)
    (cond ((null? state) #f) ; There is no disk on peg (first m)!
          ((eq? (car state) src-peg) #t)
          ((eq? (car state) dst-peg) #f)
          (else (move-check? src-peg dst-peg (cdr state)))))

(define (move-disk src-peg dst-peg state)
    (cond ((null? state) '())
          ((eq? (car state) dst-peg) state) ; Move is illegal!
          ((eq? (car state) src-peg) (cons dst-peg (cdr state))) ; Move is legal.
          (else (cons (car state) (move-disk src-peg dst-peg (cdr state))))))

(define (sequential-move move-list state)
  (if (null? move-list) ; Is the move-list empty?
      state
      (begin (let ((next-move (car move-list)))
               (src-peg (car next-move))
               (dst-peg (cadr next-move))
               (next-state (move-disk src-peg dst-peg state)))
             (if (move-check? src-peg dst-peg state)
                 (sequential-move (cdr move-list) next-state)
                 (error 'sequential-move "Illegal move.")))))

(define (agent home-peg target-peg n)
  (let ((spare-peg (car (remove-all home-peg (remove-all target-peg '(1 2 3))))))
    (if (<= n 0)
        '() ; an empty list
        (append (append (agent home-peg spare-peg (- n 1))
                        (cons (cons home-peg (cons target-peg '())) '()))
                (agent spare-peg target-peg (- n 1))))))

(define (hanoi n)
  (let ((state (initial-state n)))
    (sequential-move (agent 1 3 n) state)))

(hanoi 5)