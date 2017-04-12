#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Neveneffecten bij append}
Als je weet dat @scheme[append] in Scheme gedefinieerd is zoals hieronder, en met de volgende definities voor @scheme[l1], @scheme[l2] en @scheme[l3]:

@schemeblock[(define (append x y)
               (cond ((null? x) y)
                     (else (cons (car x)
                                 (append (cdr x) y)))))]

@defs+int[((define l1 '(1 2 3))
           (define l2 '(4 5))
           (define l3 (append l1 l2)))]

Voorspel dan de waarden van @scheme[l1], @scheme[l2] en @scheme[l3] na evaluatie van elk van de volgende expressies:

@itemize[#:style 'ordered]{@item{@interaction[(set-car! l1 6)]
                                  @itemize[#:style 'ordered]{@item{@predict[l1]}
                                                              @item{@predict[l2]}
                                                              @item{@predict[l3]}}}
                            @item{@interaction[(set-car! (cdr l3) 7)]
                                   @itemize[#:style 'ordered]{@item{@predict[l1]}
                                                               @item{@predict[l2]}
                                                               @item{@predict[l3]}}}
                            @item{@interaction[(set-car! (cdr l2) 8)]
                                   @itemize[#:style 'ordered]{@item{@predict[l1]}
                                                               @item{@predict[l2]}
                                                               @item{@predict[l3]}}}
                            @item{@interaction[(set-car! (cdddr l3) 9)]
                                   @itemize[#:style 'ordered]{@item{@predict[l1]}
                                                               @item{@predict[l2]}
                                                               @item{@predict[l3]}}}}