#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")



@title{Examen Informatica Partieel januari 1995} 
Gegeven de volgende procedure-definities:
@defs+int[((define (doe-iets x y)
               (lambda (f x)(f y)))

           (define (wat-gebeurt-er-met dit)
             (let* ((iets 5)
                    (foo (doe-iets iets dit)))
               (foo (lambda (x) x) iets))))]

Wat is dan het resultaat van de volgende procedure-aanroepen? 
(Wat doet de procedure @scheme[wat-gebeurt-er-met] dus eigenlijk?)
Verklaar je antwoord aan de hand van omgevingsmodel-diagrammen.

@predict[(wat-gebeurt-er-met 12)]
@predict[(wat-gebeurt-er-met 9)]

@solution[ @itemize[#:style 'ordered]{
        @item{ @image["hogere_orde/images/wat-gebeurt-er-met/wgem-box1.png"] }
        @item{ @image["hogere_orde/images/wat-gebeurt-er-met/wgem-box2.png"] }
        @item{ @image["hogere_orde/images/wat-gebeurt-er-met/wgem-box3.png"] }
        @item{ @image["hogere_orde/images/wat-gebeurt-er-met/wgem-box6.png"] }
        @item{ @image["hogere_orde/images/wat-gebeurt-er-met/wgem-box7.png"] }
        @item{ @image["hogere_orde/images/wat-gebeurt-er-met/wgem-box8.png"] }}]

