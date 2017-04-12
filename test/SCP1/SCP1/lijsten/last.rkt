#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 2.16 uit Abelson&Sussman}
Schrijf een procedure @scheme[last] die het laatste element van een lijst teruggeeft.

@solution{@def+int[(define (last lst)
                     (cond ((null? lst) #f)
                           ((null? (cdr lst)) (car lst))
                           (else (last (cdr lst)))))]}

@interaction[(last '(1 2 3))
             (last '(1))
             (last '())]
