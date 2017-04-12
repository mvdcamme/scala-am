#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen Informatica 2eZit 1994}
Gegeven de volgende definities:

@defs+int[((define a 2)
           (define b 4)
           
           (define (foo a)
             (let ((a (* a b))
                   (b (+ a b)))
               (display b)
               (set! a 3)
               (set! b 3)
               (display a))))]

Teken de omgevingsmodel-diagrammen voor de evaluatie van @scheme[(foo 1)].
Wat zal er uiteindelijk op het scherm worden afgedrukt?
Wat zijn de waarden van de globale variabelen a en b na afloop?

@predict[(foo 1)]

@solution[ @itemize[#:style 'ordered]{
                                      @item{ @image["objecten/images/foo/foo-box2.png"] }
                                       @item{ @image["objecten/images/foo/foo-box3.png"] }
                                       @item{ @image["objecten/images/foo/foo-box4.png"] }
                                       @item{ @image["objecten/images/foo/foo-box5.png"] }} ]