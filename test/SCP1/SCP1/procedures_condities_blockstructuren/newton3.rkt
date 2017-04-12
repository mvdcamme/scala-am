#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math @; put this ine ach file that uses formula's!

@title{Abelson and Sussman 1.8}
In de cursus wordt een voorbeeld gegeven van de
implementatie van de methode van Newton
om vierkantswortels te benaderen.
Schrijf op analoge wijze een procedure om de
derde-machtswortel van een getal te benaderen.
De methode van Newton voor derde-machtswortels
is gebaseerd op het feit dat als
y een benadering is van de derde-machtswortel van x,
een betere benadering
gegeven wordt door de formule @math-in{\frac{\frac{x}{y^2}+2y}{3}}.

@solution{@def+int[(define (derde-machtswortel x)
                     (define epsilon 0.01)
                     (define (hulp-derde-machtswortel y)
                       (if (< (abs (- (* y y y) x)) epsilon)
                           y
                           (hulp-derde-machtswortel (/ (+ (/ x (* y y)) y y) 3))))
                     (hulp-derde-machtswortel (/ x 3)))]}

@interaction[(exact->inexact (derde-machtswortel 27))]

