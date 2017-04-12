#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica Partieel januari 1994}

Schrijf een procedure @scheme[(parasol n)]
(n is een geheel getal) die een
parasolletje met gegeven hoogte op
het scherm 'tekent'.

In het volgend voorbeeld heeft het driehoekje als hoogte 5 (de waarde van de parameter n)
en als maximale breedte 9 @math-in{=(2 \times n) - 1)}.
De hoogte van het stokje is altijd 3.
Gebruik @scheme[(display " ")] en @scheme[(display "*")] om spaties, respectievelijk
sterretjes te tekenengebruik @scheme[(newline)] om een nieuwe lijn te beginnen.

@solution[@defs+int[((define (display-n n x)
                       (if (> n 0)
                           (begin
                             (display x)
                             (display-n (- n 1) x))))
                     
                     (define (parasol n)
                       (define (triangle i)
                         (if (< i n)
                             (begin
                               (display-n (- n i 1) " ")
                               (display-n (+ (* 2 i) 1) "*")
                               (newline)
                               (triangle (+ i 1)))))
                       
                       (define (stick i)
                         (if (< i 3)
                             (begin
                               (display-n (- n 1) " ")
                               (display "*")(newline)
                               (stick (+ i 1)))))
                       
                       (triangle 0)
                       (stick 0)))]]

@interaction[(parasol 5)]
