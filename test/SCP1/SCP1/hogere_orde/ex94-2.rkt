#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica 2e Zit 1994}

Gegeven de volgende Scheme-code
@defs+int[((define a 'X)
           
           (define (help)
             (define a 'Y)
             (lambda () a))
           
           (define (hulp a)
             (let ((a 'Z))
               (lambda () a))))]

Teken het omgevingsmodel-diagram na de evaluatie.

@solution[@image["hogere_orde/images/hulp/hulp-box2.png"]]

Schrijf drie expressies die aan de hand van bovenstaande definities respectievelijk de symbolen X, Y en Z teruggeven. Argumenteer je
antwoord aan de hand van omgevingsdiagrammen.
@solution[@interaction[a
                       ((help))
                       ((hulp 'om-het-even-wat))]]

@solution[ @itemize[#:style 'ordered]{@item{ @image["hogere_orde/images/hulp/hulp-box4.png"] }
                                      @item{ @image["hogere_orde/images/hulp/hulp-box5.png"] }
                                      @item{ @image["hogere_orde/images/hulp/hulp-box6.png"] }
                                      @item{ @image["hogere_orde/images/hulp/hulp-box9.png"] }
                                      @item{ @image["hogere_orde/images/hulp/hulp-box10.png"] }
                                      @item{ @image["hogere_orde/images/hulp/hulp-box11.png"] }}]
                                       
                                       
                                       