#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Nog meer definities van procedures (oefening 4.9 en 4.10 uit Simply Scheme)}
@section{Discount}
Schrijf een procedure @scheme[(discount prijs korting)] die twee argumenten heeft:
de initiëele prijs van een item en het kortingspercentage.
De procedure moet de nieuwe prijs teruggeven:

@solution[@def+int[(define (discount prijs korting)
                     (* prijs (/ (- 100 korting) 100.0)))]]

@interaction[(discount 10 5)(discount 29.90 50)]

@section{Fooi (tip)}
Schrijf een procedure @scheme[(tip bedrag)] om de fooi te berekenen die je
in een restaurant zou geven. Deze procedure neemt als argument het
totaalbedrag en geeft het fooibedrag terug.
De fooi is 15% van het totale bedrag maar de procedure moet
kunnen afronden zodanig dat het totale geldbedrag (dus de originele prijs plus fooi)
een geheel aantal Euro's is. (Gebruik de procedure @scheme[ceiling] om af te ronden.)

@solution[@def+int[(define (tip bedrag)
                     (- (ceiling(+ bedrag (* 0.15 bedrag))) bedrag))]]

@interaction[(tip 19.98)(tip 29.23)(tip 7.54)]
