#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen Informatica 2eZit 1997}
Gegeven de volgende definities:

@defs+int[((define a 2)
           (define b 4)
           
           (define (foo bar a)
             (let* ((a (bar a b))
                    (b (bar a b)))
               (display b))
             (set! a 3)
             (set! b 3)
             (display a)))]

@section{Omgevingsmodel-diagrammen}
Teken de omgevingsmodel-diagrammen voor de evaluatie van @scheme[(foo + 1)] en @scheme[(foo * 2)] indien deze na elkaar geëvalueerd worden. Welke getallen zullen uiteindelijk op het scherm worden afgedrukt?

@itemize[#:style 'ordered]{@item{@predict[(foo + 1)]}
                            @item{@predict[(foo * 2)]}}

@solution[ @itemize[#:style 'ordered]{
                                      @item{ @image["objecten/images/foo2/foo2-box2.png"] }
                                      @item{ @image["objecten/images/foo2/foo2-box3.png"] }
                                      @item{ @image["objecten/images/foo2/foo2-box4.png"] }
                                      @item{ @image["objecten/images/foo2/foo2-box5.png"] }
                                      @item{ @image["objecten/images/foo2/foo2-box10.png"] }
                                      @item{ @image["objecten/images/foo2/foo2-box11.png"] }
                                      @item{ @image["objecten/images/foo2/foo2-box12.png"] }}]

@section{Voorspel}
Wat gebeurt er als je vervolgens @scheme[(foo 2 4)] evalueert?

@predict[(foo 2 4)]

