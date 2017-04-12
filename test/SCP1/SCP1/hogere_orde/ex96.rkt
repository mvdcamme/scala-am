#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math 

@title{Examen Informatica 1e Zit 1996}
Albert heeft een functie power geschreven om de n-de macht van een getal a te bepalen:
@def+int[(define (power a)
           (define (loop ctr pow)
             (if (> ctr n)
                 pow
                 (loop (+ ctr 1) (* pow a))))
           (loop 1 1))]

Om zijn functie te testen probeert Albert het volgende, wat het verwachte resultaat geeft.

@def+int[(define n 3)]

@solution[@interaction[(power 5)]]

Vervolgens gebruikt Albert zijn functie power in een functie f die als volgt gedefinieerd is:
@def+int[(define (f x y z)
           (let ((n (* y z)))
             (power x)))]

Als Albert deze functie nu oproept met argumenten @scheme[(f 6 2 4)] verwacht hij dat het resultaat
gelijk is aan @math-in{6^{2*4}}. Nochtans krijgt hij een totaal ander resultaat.

@section{Voorspel}
Welk resultaat krijgt Albert dan wel?
@predict[(f 6 2 4)]

@section{Verklaar}
Verklaar dit resultaat aan de hand van omgevingsmodel-diagrammen.


@solution[ @itemize[#:style 'ordered]{
     @item{ @image["hogere_orde/images/power/power-box2.png"] }
     @item{ @image["hogere_orde/images/power/power-box3.png"] }
     @item{ @image["hogere_orde/images/power/power-box4.png"] }
     @item{ @image["hogere_orde/images/power/power-box5.png"] }
     @item{ @image["hogere_orde/images/power/power-box6.png"] }
     @item{ @image["hogere_orde/images/power/power-box7.png"] }
     @item{ @image["hogere_orde/images/power/power-box8.png"] }
     @item{ @image["hogere_orde/images/power/power-box9.png"] }}]

@section{Leg uit}
Leg in een drietal zinnen uit wat Albert precies verkeerd heeft gedaan.
