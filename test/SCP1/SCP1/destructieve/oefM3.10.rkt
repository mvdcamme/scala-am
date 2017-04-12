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

@title{Oefening M3.10 uit Instructors manual van Abelson&Sussman}
Schrijf een functie @scheme[(left-rotate r)] die een circulaire lijst 1 plaats naar links verschuift.

@solution{@defs+int[((define (left-rotate r)
                       (cdr r)))]}

@interaction[(define r (make-ring 3))]
@interaction[(print-ring (left-rotate r))]