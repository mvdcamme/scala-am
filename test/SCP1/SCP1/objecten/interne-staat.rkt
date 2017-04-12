#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Interne staat}

@section[#:tag "vraag1"]{Flip}
Definieer een procedure @scheme[(flip)] die 1 teruggeeft bij de eerste oproep, 0 bij de tweede oproep, 1 bij de derde oproep, enzovoort.

@solution[@def+int[(define flip
                     (let ((state 0))
                       (lambda ()
                         (if (= state 0)
                             (set! state 1)
                             (set! state 0))
                         state)))]]

@interaction[(flip)
             (flip)
             (flip)
             (flip)]


@section{Omgevingsmodel-diagram voor @scheme[flip]}
Gebruik het omgevingsmodel om elke oproep van @scheme[flip] uit @secref{vraag1} te analyseren.

@solution[ @itemize[#:style 'ordered]{
                                      @item{ @image["objecten/images/flip/flip-box2.png"] }
                                       @item{ @image["objecten/images/flip/flip-box3.png"] }
                                       @item{ @image["objecten/images/flip/flip-box6.png"] }
                                       @item{ @image["objecten/images/flip/flip-box9.png"] }
                                       @item{ @image["objecten/images/flip/flip-box11.png"] }}]

@section{Make-flip}
Schrijf een procedure @scheme[(make-flip)] die kan gebruikt worden op de procedure @scheme[flip] als volgt te definiëren: @scheme[(define flip (make-flip))].

@solution[@def+int[(define (make-flip)
                     (let ((state 0))
                       (lambda ()
                         (if (= state 0)
                             (set! state 1)
                             (set! state 0))
                         state)))]]

@section{Voorspel}
Gegeven de volgende definities.
@defs+int[((define flip (make-flip))
           (define flap1 (flip))
           (define (flap2) (flip))
           (define flap3 flip)
           (define (flap4) flip))]

Wat is dan de waarde van de volgende expressies (indien ze @emph{in deze volgorde} worden geëvalueerd)?

@itemize[#:style 'ordered]{@item{@predict[flap1]}
                            @item{@predict[flap2]}
                            @item{@predict[flap3]}
                            @item{@predict[flap4]}
                            @item{@predict[(flap1)]}
                            @item{@predict[(flap2)]}
                            @item{@predict[(flap3)]}
                            @item{@predict[(flap4)]}
                            @item{@predict[flap1]}
                            @item{@predict[(flap3)]}
                            @item{@predict[(flap2)]}}


