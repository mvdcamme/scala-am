#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (double x) (+ x x))]
@hidden-code[(define (halve x) (/ x 2))]

@title{Simulatie}
Schrijf twee functies die door middel van simulatie berekenen hoeveel
recursieve oproepen gebeuren in de procedures uit vraag 2.


@solution[@defs+int[((define (sim-multiply a b)
                       (if (zero? b)
                           1
                           (+ 1 (sim-multiply a (- b 1)))))
                     (define (sim-fast-multiply a b)
                       (cond ((zero? b) 1)
                             ((even? b) (+ 1 (sim-fast-multiply (double a) (halve b))))
                             (else (+ 1 (sim-fast-multiply a (- b 1)))))))]]

@interaction[(sim-multiply 14 2365)(sim-fast-multiply 14 2365)]
