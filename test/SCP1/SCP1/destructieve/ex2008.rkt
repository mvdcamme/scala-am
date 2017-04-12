#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen januari 2008: Destructief}
Schrijf een destructieve procedure @scheme[all-but-interval] die een stijgend
geordende lijst van getallen neemt en alle getallen in een gegeven interval
verwijdert. Er worden twee parameters doorgegeven om het interval aan te
duiden, en de eerste is kleiner of gelijk aan de tweede.


@solution{@defs+int[((define (all-but-interval lst min max)
                       (define (aux last-smaller-cons aux-lst)
                         (cond
                           ((null? aux-lst)
                            (set-cdr! last-smaller-cons '()))
                           ((< (car aux-lst) min)
                            (aux aux-lst (cdr aux-lst)))
                           ((> (car aux-lst) max)
                            (set-cdr! last-smaller-cons aux-lst))
                           (else
                            (aux last-smaller-cons (cdr aux-lst)))))
                       (aux lst lst)
                       lst))]}

@interaction[(all-but-interval '(1 2 3 4 5 6) 2 4)]
@interaction[(all-but-interval '(1 2 3 4 5) 2 2)]
@interaction[(all-but-interval '(1 2 5 6 7) 3 9)]