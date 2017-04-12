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
Schrijf een functie @scheme[(right-rotate r)] die een circulaire lijst 1 plaats naar rechts verschuift.

@solution{@defs+int[((define (right-rotate r)
                       (define (iter l)
                         (if (eq? (cdr l) r)
                             l
                             (iter (cdr l))))
                       (iter r)))]
           
           Merk op dat deze procedure alleen werkt voor echte ringen, en niet voor eindige structuren zoals de volgende:
           @interaction[(right-rotate '(1 2 3))]}

@interaction[(define r (make-ring 3))]
@interaction[(print-ring (right-rotate r))]