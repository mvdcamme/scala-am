#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 2.4 uit Abelson&Sussman}

@section{Definitie van my-cdr}
Geef de definitie van @scheme[my-cdr] indien @scheme[my-cons] en @scheme[my-car] als volgt gedefinieerd zijn:
@defs+int[((define (my-cons x y)
             (lambda (m) (m x y)))
           
           (define (my-car z)
             (z (lambda (p q) p))))]

@solution{@def+int[(define (my-cdr z)
                     (z (lambda (p q) q)))]}

@section{Omgevingsmodel voor @scheme[my-cons]}
Teken een omgevingsmodel-diagram voor evaluatie van de volgende expressie
@def+int[(define a (my-cons 1 2))]

@solution[@image["lijsten/images/my-cons/mc-box4.png"]]

@section{Voorspel}
Wat gebeurt er nu bij evaluatie van 
@itemize[@item[@predict[(my-car a)]]
          @item[@predict[(my-cdr a)]]]

@solution[ @itemize[#:style 'ordered]{
     @item{ @image["lijsten/images/my-cons/mc-box5.png"] }
     @item{ @image["lijsten/images/my-cons/mc-box6.png"] }}]

