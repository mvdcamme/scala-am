#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica Partieel januari 1994}
Boudewijn moet een programma schrijven waarin hij dikwijls elk element van een volledige lijst moet ophogen met een bepaald getal. Boudewijn herinnert zich de @scheme[map-over-list] uit de cursus en schrijft nog een functie @scheme[add-n] die n toevoegt bij een gegeven getal.

@defs+int[((define (map-over-list a-list a-function)
             (cond ((null? a-list) '())
                   (else (cons (a-function (car a-list))
                               (map-over-list (cdr a-list) a-function)))))
           
           (define add-n (lambda (x) (+ x n))))]

Boudewijn is een voorzichtig baasje en probeert het geheel eens uit zoals hieronder. Hij is tevreden, het werkt.

@interaction[(define n 2)
             (map-over-list '(1 2 3 4 5) add-n)]

Boudewijn verwerkt dit nu in zijn groot programma zoals hieronder geschetst. Hij is niet tevreden, het werkt niet.

@def+int[(define (groot-programma the-list a b c)
           ; ... 
           (let ((n (+ a b c)))
             (map-over-list the-list add-n))
           ; ...
           )]

Na lang zoeken en prutsen komt Boudewijn tot de vaststelling dat het stukje programma dat elk element van een lijst zou moeten ophogen met n het blijkbaar niet doet. Om het even welke getallen die hij bij de oproep van zijn programma voor a, b, en c gebruikt, de lijstelementen worden altijd opgehoogd met 2.

Leg jij eens uit wat er verkeerd loopt. Gebruik daarvoor het omgevingsmodel voor evaluatie. Wij verwachten hier dus een aantal tekeningen plus wat begeleidende tekst.


@solution[ @itemize[#:style 'ordered]{
     @item{ @image["lijsten/images/groot-programma/gp-box3.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box4.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box5.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box6.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box7.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box9.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box10.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box12.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box13.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box15.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box16.png"] }
     @item{ @image["lijsten/images/groot-programma/gp-box18.png"] }}]