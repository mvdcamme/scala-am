(define (assert p)
  (if (not p) (error "assertion failed")))

(define (main)
  (display "Enter thresholds for A, B, C, D\nin that order, decreasing percentages > ")
  (define a (random 1))
  (define b (random 1))
  (define c (random 1))
  (define d (random 1))
  (newline)
  (display "Thank you. Now enter student score (percent) >")
  (define score (random 1))
  (assert (< a b))
  (assert (< a c))
  (assert (< a d))
  (assert (< b c))
  (assert (< b d))
  (assert (< c d))
  (cond ((>= score a) (display "Student has an A grade\n"))
        ((>= score b) (display "Student has a B grade\n"))
        ((>= score c) (display "Student has a C grade\n"))
        ((>= score d) (display "Student has a D grade\n"))
        (else (display "Student has an F grade\n"))))
(main)