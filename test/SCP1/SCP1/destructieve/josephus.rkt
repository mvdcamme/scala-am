#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (make-ring n)
               (let ((last (cons 0 '())))
                 (define (build-list n)
                   (if (= n 0)
                       last
                       (cons n (build-list (- n 1)))))
                 (let ((ring (build-list n)))
                   (set-cdr! last ring)
                   ring)))]
@hidden-code[(define (print-ring r)
               (define (aux l)
                 (if (not (null? l))
                     (if (eq? (cdr l) r)
                         (begin (display " ") (display (car l)) (display "..."))
                         (begin (display " ") (display (car l)) (aux (cdr l))))))
               (aux r)
               #t)]
@hidden-code[(define (copy-ring r)
               (define last '())
               (define (aux l)
                 (cond ((eq? (cdr l) r) 
                        (set! last (cons (car l) '()))
                        last)
                       (else (cons (car l) (aux (cdr l))))
                       ))
               (let ((first (aux r)))
                 (set-cdr! last first)
                 first))]

@title{Josephus}
Schrijf een functie @scheme[(Josephus r n)] die een circulaire lijst afloopt en telkens het @scheme[n]-de element verwijdert, totdat er slechts 1 element over is. Dat laatste wordt als resultaat teruggegeven. Je mag procedures gebruiken die we hierboven gedefinieerd hebben.

@solution{@defs+int[((define (Josephus r n)
                       (define (remove-nth! l n)
                         (if (<= n 2)
                             (begin (set-cdr! l (cddr l))
                                    (cdr l))
                             (remove-nth! (cdr l) (- n 1))))
                       
                       (define (iter l)
                         (print-ring l)
                         (if (eq? l (cdr l))
                             (car l)
                             (iter (remove-nth! l n))))
                       
                       (if (= n 1)
                           (car (right-rotate r))
                           (iter (copy-ring r)))))]}

@interaction[(define ring (make-ring 5))]
@interaction[(Josephus ring 5)]
@interaction[(print-ring ring)]