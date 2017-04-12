#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Lussen}
Schrijf een functie @scheme[(make-ring n)] die een positief geheel getal neemt en er een ring van maakt (een ring is een circulaire lijst van n tot 0).

@solution{@defs+int[((define (make-ring n)
                       (let ((last (cons 0 '())))
                         (define (build-list n)
                           (if (= n 0)
                               last
                               (cons n (build-list (- n 1)))))
                         (let ((ring (build-list n)))
                           (set-cdr! last ring)
                           ring))))]}

De volgende hulpprocedure is een procedure die er (gelukkig) in slaagt een constructie met oneindige lussen op eindige manier af te drukken.

@defs+int[((define (print-ring r)
             (define (aux l)
               (if (not (null? l))
                   (if (eq? (cdr l) r)
                       (begin (display " ") 
                              (display (car l)) 
                              (display "..."))
                       (begin (display " ") 
                              (display (car l)) 
                              (aux (cdr l))))))
             (aux r)
             #t))]

@interaction[(define r (make-ring 3))]
@interaction[(print-ring r)]
@interaction[(print-ring (cdr r))]
