#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Unfringe}
Definieer de procedure @scheme[(unfringe l)] die 
een platte lijst als argument neemt en
een lijst teruggeeft die, net als al haar
sublijsten, maximaal 2 elementen telt en
terug @scheme[l] geeft als je er @scheme[fringe] (uit vraag 3)
op los laat.

Opmerking: Vermits verschillende lijsten
hetzelfde resultaat kunnen opleveren met
fringe, is het resultaat van unfringe niet
eenduidig bepaald)

@solution{Eerste manier:
          @def+int[(define (unfringe-1 l)
                     (cond ((null? l) '())
                           ((null? (cdr l)) (list (car l)))
                           (else (list (car l) 
                                       (unfringe-1 (cdr l))))))]
          Tweede manier:
          
          @def+int[(define (unfringe-2 l)
                     (define (pair l)
                       (cond ((null? l) '())
                             ((null? (cdr l)) (list l))
                             (else (cons (list (car l) (cadr l))
                                         (pair (cddr l))))))
                     
                     (let loop ((l l))
                       (if (or (null? l)
                               (null? (cdr l)))
                           l
                           (loop (pair l)))))]}

@interaction[(unfringe-1 '(1 2 3 4 5 6 7 8 9))
             (unfringe-2 '(1 2 3 4 5 6 7 8 9))]
