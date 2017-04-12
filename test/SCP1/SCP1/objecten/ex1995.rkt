#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen Informatica 1eZit 1995}
Gegeven de volgende definities

@defs+int[((define hulp 2)
           
           (define (haha x)
             (let ((hulp (* x hulp)))
               (display hulp))
             (display hulp)
             (set! hulp 4)))]

Voorspel dan de output van de volgende expressies aan de hand van omgevingsmodel-diagrammen.

@itemize[#:style 'ordered]{@item{@predict[(haha 2)]}
                            @item{@predict[(haha 3)]}}

@solution[ @itemize[#:style 'ordered]{
                                      @item{ @image["objecten/images/haha/haha-box1.png"] }
                                       @item{ @image["objecten/images/haha/haha-box2.png"] }
                                       @item{ @image["objecten/images/haha/haha-box3.png"] }
                                       @item{ @image["objecten/images/haha/haha-box7.png"] }
                                       @item{ @image["objecten/images/haha/haha-box8.png"] }}]



