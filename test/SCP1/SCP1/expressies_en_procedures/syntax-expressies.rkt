#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Syntax en evaluatie van expressies}
@section{Voorspel het resultaat van enkele eenvoudige expressies}
Voorspel (zonder gebruik te maken van een Scheme vertolker) voor 
elk van onderstaande expressies het resultaat indien ze in de gegeven
(alfabetische) volgorde geëvalueerd worden.
Leg de gevallen uit waarbij het misloopt.

Controleer nadien, door middel van een
Scheme vertolker, of je prognoses correct was. Probeer de fouten die je gemaakt hebt te 
begrijpen en uit te leggen.

@itemize[#:style 'ordered]{@item{@predict[(* (+ 2 2) 5)]}
                            @item{@predict[(* (+ 2 2) (5))]}
                            @item{@predict[(*(+(2 2) 5))]}
                            @item{@predict[(*(+ 22)5)]}
                            @item{@predict[(5 * 4)]}
                            @item{@predict[5*4]}
                            @item{@predict[(define 5*4 20)]}
                            @item{@predict[5*4]}
                            @item{@predict[(5 * 2+2)]}
                            @item{@predict[(* 5 (2+2))]}
                            @item{@predict[(* 5 (2 + 2))]}
                            @item{@predict[(5*2 + 2)]}
                            @item{@predict[(5*4 + 2)]}
                            @item{@predict[(5* (+ 2 2))]}
                            @item{@predict[((+ 2 3))]}
                            @item{@predict[(/ 6 2)]}
                            @item{@predict[/]}
                            @item{@predict[(define $   /)]}
                            @item{@predict[(define / (* 2 3))]}
                            @item{@predict[(/ 6 2)]}
                            @item{@predict[/]}
                            @item{@predict[($  6 2)]}
                            @item{@predict[$]}
                            @item{@predict[(* 2 /)]}
                            @item{@predict[(define(double x)(+ x x))]}
                            @item{@predict[(double (double 5))]}
                            @item{@predict[(define (five) 5)]}
                            @item{@predict[(define four 4)]}
                            @item{@predict[four]}
                            @item{@predict[five]}
                            @item{@predict[(four)]}
                            @item{@predict[(five)]}}

@section{Omgevingsmodellen voor eenvoudige expressies}
Doe nu hetzelfde voor de volgende expressies en maak daarenboven
gebruik van omgevingsmodellen om de resultaten te bespreken.
@itemize[#:style 'ordered]{@item{@scheme[(define $   /)] 
                                 @solution[ @image["expressies_en_procedures/images/voorspel/voorspel-box2.png"] ]}
                            @item{@scheme[(define / (* 2 3))]
                                   @solution[@image["expressies_en_procedures/images/voorspel/voorspel-box3.png"] ]}
                            @item{@scheme[(/ 6 2)]}
                            @item{@scheme[/]}
                            @item{@scheme[($  6 2)]}
                            @item{@scheme[$]}
                            @item{@scheme[(* 2 /)]}
                            @item{@scheme[(define(double x)(+ x x))]
                                  @solution[ @image["expressies_en_procedures/images/voorspel/voorspel-box8.png"] ]}
                            @item{@scheme[(double (double 5))]
                                  @solution[ @image["expressies_en_procedures/images/voorspel/voorspel-box9.png"] ]}
                            @item{@scheme[(define (five) 5)]
                                  @solution[ @image["expressies_en_procedures/images/voorspel/voorspel-box10.png"] ]}
                            @item{@scheme[(define four 4)]
                                  @solution[ @image["expressies_en_procedures/images/voorspel/voorspel-box12.png"] ]}
                            @item{@scheme[four]}
                            @item{@scheme[five]}
                            @item{@scheme[(four)]}
                            @item{@scheme[(five)]
                                  @solution[ @image["expressies_en_procedures/images/voorspel/voorspel-box16.png"] ]}}

                                      

@(reset-r5rs-evaluator)