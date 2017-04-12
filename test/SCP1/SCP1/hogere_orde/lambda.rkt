#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Lambda notatie voor procedures}
@section{lambda}
Herdefinieer de procedure @scheme[check-db0-my-if] uit deel (d) van vraag 3
door op de plaats waar my-if voorkomt rechtstreeks de overeenkomstige lambda functie te gebruiken.

@section{Compose}
Schrijf een algemene procedure @scheme[compose],
die twee procedures @scheme[f] en
@scheme[g] van 1 enkele parameter @scheme[x] als invoer neemt,
en als uitvoer de samengestelde functie @math-in{f \circ g} teruggeeft.

@solution[@def+int[(define (compose f g)
                     (lambda (x) (f (g x))))]]

@interaction[(define (f x) (* x x))
             (define (g x) (+ x 2))
             (f (g 3))
             (compose f g)
             ((compose f g) 3)]
