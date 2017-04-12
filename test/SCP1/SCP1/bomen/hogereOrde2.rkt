#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Hogere Orde}

Definieer de functie @scheme[(tree-accumulate tree term combiner null-value)],
die werkt zoals @scheme[accumulate], maar de functie @scheme[term] alleen
loslaat op atomen, en de @scheme[combiner] gebruikt om de accumulatie van
deellijsten te combineren.

@solution[@def+int[(define (tree-accumulate tree term combiner null-value)
                     (cond ((null? tree) null-value)
                           ((atom? tree) (term tree))
                           (else (combiner (tree-accumulate (car tree) 
                                                            term 
                                                            combiner 
                                                            null-value)
                                           (tree-accumulate (cdr tree) 
                                                            term 
                                                            combiner 
                                                            null-value)))))]]


@section{Hergebruik}
Definieer fringe, deep-combine en deep-map aan de hand van tree-accumulate.

@solution[@defs+int[((define (fringe l)
                       (tree-accumulate l list append '()))
                     
                     (define (deep-combine combiner null-value l)
                       (tree-accumulate l 
                                        (lambda (x) x) 
                                        combiner 
                                        null-value))
                     
                     (define (deep-map f l)
                       (tree-accumulate l f cons '())))]]


@section{Display}
Kun je ook de procedure @scheme[display-list] schrijven aan de hand van
@scheme[tree-accumulate]?