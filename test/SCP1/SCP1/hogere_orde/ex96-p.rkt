#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica Partieel januari 1996}
An Ticipatie heeft de volgende procedure add-one geschreven om bij een getal 1 bij te tellen.
@defs+int[((define one 1)
           
           (define (add-one x)
             (+ x one)))]

Vervolgens wenst ze deze procedure te hergebruiken in twee analoge procedures add-two en add-three om 2 respectievelijk 3 bij een getal bij te tellen. Ze doet dit als volgt:
@defs+int[((define (add-two x)
             (let ((one 2))
               (add-one x)))
           
           (define (add-three x)
             (define one 1)
             (+ (add-two one) x)))]

In de veronderstelling dat al deze definities na
elkaar in de globale omgeving hebben plaatsgevonden,
wat is dan het resultaat van elk van de volgende
expressies? Verklaar en motiveer je antwoord aan de
hand van omgevingsmodel-diagrammen.
@predict[(add-one 5)]
@predict[(add-two 5)]
@predict[(add-three 5)]


@solution[ @itemize[#:style 'ordered]{
     @item{ @image["hogere_orde/images/add/add-box3.png"] }
     @item{ @image["hogere_orde/images/add/add-box4.png"] }
     @item{ @image["hogere_orde/images/add/add-box7.png"] }
     @item{ @image["hogere_orde/images/add/add-box8.png"] }
     @item{ @image["hogere_orde/images/add/add-box9.png"] }
     @item{ @image["hogere_orde/images/add/add-box14.png"] }
     @item{ @image["hogere_orde/images/add/add-box15.png"] }
     @item{ @image["hogere_orde/images/add/add-box16.png"] }
     @item{ @image["hogere_orde/images/add/add-box17.png"] }}]
