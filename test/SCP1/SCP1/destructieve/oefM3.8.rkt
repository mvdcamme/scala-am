#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening M3.8 uit Instructors manual van Abelson&Sussman}
Gegeven de definities:
@defs+int[((define x (list '(a b) '(c d)))
           (define y (cons x (cdr x))))]

Geef dan het resultaat van de evaluatie (in volgorde) van volgende expressies (teken box-pointer diagrammen):

@itemize[#:style 'ordered]{@item{@predict[x]
                                  @solution{@image["destructieve/images/box-pointer-x.png" #:scale 0.14]}}
                           @item{@predict[y]
                                    @solution{@image["destructieve/images/box-pointer-y.png" #:scale 0.14]}}
                            @item{@predict[(car (car y))]}
                            @item{@predict[(set-car! (car y) (cdr y))]
                                    @solution{@image["destructieve/images/box-pointer-y2.png" #:scale 0.14]}}
                            @item{@predict[x]}
                            @item{@predict[y]}}