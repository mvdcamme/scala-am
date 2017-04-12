#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math @; put this ine ach file that uses formula's!

@title{Omgevingsmodellen voor let statement}

@section[#:tag "let:lambda"]{Lambda}
Leg uit wat er mis gaat bij de evaluatie van de volgende expressie.
Hint: Zet de @scheme[let] om in de overeenkomstige lambda expressie en teken dan het omgevingsmodel-diagram.

@schemeblock[(let ((x 1)
                   (y (+ 1 x)))
               (+ x y))]

@solution[@interaction[((lambda (x y) (+ x y)) 1 (+ 1 x))]]

@section[#:tag "let:omgevingsmodel"]{Omgevingsmodel-diagrammen}
Voorspel de output van @scheme[(foo 1 2 3)] aan de hand van omgevingsmodel-diagrammen.

@defs+int[((define (print-abc a b c)
             (display a) (display " ")
             (display b) (display " ")
             (display c) (newline))
           
           (define (foo a b c)
             (print-abc a b c)
             (let ((a 4)
                   (c 5)
                   (b c))
               (print-abc a b c)
               (let ((b 6)
                     (c a))
                 (print-abc a b c))
               (let ((a b)
                     (c a))
                 (print-abc a b c)))
             (print-abc a b c)))]

@solution[@interaction[(foo 1 2 3)]]

