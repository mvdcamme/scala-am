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

@title{Copy ring}
Schrijf een functie @scheme[copy-ring] die een kopie van een circulaire lijst teruggeeft.

@solution{@defs+int[((define (copy-ring r)
                       (define last '())
                       (define (aux l)
                         (cond ((eq? (cdr l) r) 
                                (set! last (cons (car l) '()))
                                last)
                               (else (cons (car l) (aux (cdr l))))
                               ))
                       (let ((first (aux r)))
                         (set-cdr! last first)
                         first)))]}

@interaction[(define r (make-ring 3))]
@interaction[(define s (copy-ring r))]
@interaction[(print-ring s)]
@interaction[(set-car! s 999)]
@interaction[(print-ring s)]
@interaction[(print-ring r)]
@interaction[(set-car! (cdr s) 888)]
@interaction[(print-ring s)]
@interaction[(print-ring r)]
