#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Oefening 2.28 uit Abelson&Sussman}
Definieer de procedure @scheme[(fringe l)]
die een lijst teruggeeft met alle atomen
van een diepgeneste lijst.

@solution[@def+int[(define (fringe l)
                     (cond ((null? l) '())
                           ((atom? l) (list l))
                           (else (append (fringe (car l))
                                         (fringe (cdr l))))))]]


@interaction[(fringe '((1) ((((2)))) (3 (4 5) 6) ((7) 8 9)))]
