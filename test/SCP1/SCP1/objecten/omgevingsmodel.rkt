#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Objecten en het omgevingsmodel}
Laurent vindt in een software-bibliotheek een procedure om "rechthoek"-objecten te maken:

@defs+int[((define (maak-rechthoek l b)
             (define (oppervlakte) (* l b))
             (define (omtrek) (* 2 (+ l b)))
             (define (dispatch m)
               (cond ((eq? m 'oppervlakte) (oppervlakte))
                     ((eq? m 'omtrek) (omtrek))))
             dispatch))]

Hij moet als oefening een object @scheme[vierkant] aanmaken dat herschaalbaar is. Hij herinnert zich dat een vierkant een rechthoek is met vier gelijke zijden en bedenkt daarom de volgende oplossing:

@defs+int[((define (maak-vierkant zijde)
             (define rechthoek (maak-rechthoek zijde zijde))
             (define (schaal! n) (set! zijde (* n zijde)))
             (define (dispatch m)
               (cond ((eq? m 'oppervlakte) (rechthoek 'oppervlakte))
                     ((eq? m 'omtrek) (rechthoek 'omtrek))
                     ((eq? m 'schaal!) schaal!)))
             dispatch))]

Op het eerste zicht lukt dat, zoals blijkt uit de volgende tests:
@itemize{@item[@interaction[(define test (maak-vierkant 5))]]
         @item[@interaction[(test 'oppervlakte)]]
         @item[@interaction[(test 'omtrek)]]}

Maar bij nader inzien toch niet: na het schalen met een factor 2 blijft de oppervlakte 25:
@itemize{@item[@interaction[((test 'schaal!) 2)]]
         @item[@interaction[(test 'oppervlakte)]]}

Hoe komt dit? Teken zorgvuldig omgevingsmodellen die uitleggen wat er gebeurt bij de aanmaak van het vierkant, bij de vraag om de oppervlakte te berekenen, en bij de vraag om te schalen. Leg dan met een paar zinnen uit waarom het antwoord op de laatste vraag 25 is i.p.v. 100.

@solution{@itemize[#:style 'ordered]{
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box1.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box2.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box3.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box4.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box5.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box6.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box7.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box9.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box12.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box13.png"] }
      @item{ @image["objecten/images/omgevingsmodel/omgOO-box14.png"] }}}
