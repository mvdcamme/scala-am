#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen Informatica 1eZit 1994}
@section{Omgevingsmodel-diagram}
Teken een omgevingsmodel-diagram voor de situatie na evaluatie van het volgend stuk Scheme code:

@defs+int[((define (subtract-ct n c)
             (set! n (- n c))
             n)
           
           (define (make-sub i)
             (lambda (p) (subtract-ct p i)))
           
           (define sub1 (make-sub 1))
           
           (define x 5))]

@solution[ @image["objecten/images/subtract/subtract-box5.png"] ]

@section{Verklaar}
Wat is het resultaat van de evaluatie van de volgende expressies? Verklaar je antwoord aan de hand van omgevingsmodel-diagrammen.

@itemize[#:style 'ordered]{@item{@predict[(sub1 x)]}
                           @item{@predict[x]}}

@solution[ @itemize[#:style 'ordered]{
       @item{ @image["objecten/images/subtract/part2/subtract-box6.png"] }
       @item{ @image["objecten/images/subtract/part2/subtract-box7.png"] }
       @item{ @image["objecten/images/subtract/part2/subtract-box8.png"] }
       @item{ @image["objecten/images/subtract/part2/subtract-box11.png"] }} ]