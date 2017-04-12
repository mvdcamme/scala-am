#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math @; put this line in each file that uses math formula's!

@title{Vierkantsvergelijking}

@section{}
Schrijf een procedure @scheme[(make-vkv a b c)], die als parameters
de coëfficienten a, b en c van een vierkantsvergelijking
van de vorm @math-in{ax^2+bx+c} heeft en de vierkantsvergelijking
als functie van één enkele parameter x teruggeeft.

@solution[@def+int[(define (make-vkv a b c)
                     (lambda (x)
                       (+ (* a x x)
                          (* b x)
                          c)))]]

@section{}
Definieer nu een vierkantsvergelijking @math-in{vkv(x)=3x^2+5x+1} m.b.v.
deze functie @scheme[make-vkv], en teken het omgevingsmodel-diagram
voor de oproep @scheme[(make-vkv 3.14)]

@solution[@def+int[(define vkv (make-vkv 3 5 1))
                   (vkv 3.14)
                   omgevings-model-ontbreekt]]
