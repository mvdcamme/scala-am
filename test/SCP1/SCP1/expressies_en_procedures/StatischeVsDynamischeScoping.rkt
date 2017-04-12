#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Statische versus dynamische scoping}
@section{omgevingsmodel-diagram}
Teken het omgevingsmodel-diagram voor de volgende definities:

@itemize{@item{@def+int[(define c 3)]}
          @item{@def+int[(define (add-c x y)
                           (+ x y c))]}
          @item{@def+int[(define (sum x y c)(add-c x y))]}}


@section{Voorspel het resultaat van @scheme{sum}}
Voorspel het resultaat van volgende expressie aan de hand van
omgevingsmodel-diagrammen.

@predict[(sum 1 2 6)]

@solution{@itemlist[#:style 'ordered
                            @item{ @image["expressies_en_procedures/images/scoping/scoping-box2.png"] }
                            @item{ @image["expressies_en_procedures/images/scoping/scoping-box5.png"] }
                            @item{ @image["expressies_en_procedures/images/scoping/scoping-box6.png"] }]}

@section{Dynamische scoping}
Wat zou het resultaat van @scheme[(sum 1 2 6)]
geweest zijn indien Scheme een taal met dynamische
(i.p.v. statische) scoping was?
@solution[@interaction[(+ 1 2 6)]]


