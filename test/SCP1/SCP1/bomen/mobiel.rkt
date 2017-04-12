#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Oefening 2.29 uit Abelson&Sussman}

Een mobiel (zo'n ding dat je aan het plafond hangt en dat met de tocht beweegt),
is ofwel een dom gewicht (getal),
ofwel een koppel van twee armen,
die elk een bepaalde lengte hebben
en waaraan weer een mobiel hangt.
Je kan een mobiel construeren door
@scheme[(make-mobile length1 weight1 length2 weight2)],
waarbij @scheme[weight1] en @scheme[weight2] ofwel getallen zijn (gewichten),
ofwel weer mobielen.

@section{Constructor en selectoren}
Definieer deze constructor en de selectoren.
@solution[@defs+int[((define (make-mobile length1 weight1 length2 weight2)
                       (list length1 weight1 length2 weight2))
                     
                     (define (length1 mobile) (list-ref mobile 0))
                     
                     (define (weight1 mobile) (list-ref mobile 1))
                     
                     (define (length2 mobile) (list-ref mobile 2))
                     
                     (define (weight2 mobile) (list-ref mobile 3)))]]


@section{Gewicht van een mobiel}
Definieer @scheme[(mobile-weight m)], die het totaal gewicht van het mobiel teruggeeft
(het gewicht van de takken is verwaarloosbaar).
@solution[@def+int[(define (mobile-weight mobile)
                     (if (atom? mobile)
                         mobile
                         (+ (mobile-weight (weight1 mobile))
                            (mobile-weight (weight2 mobile)))))]]


@section{Gebalanceerd?}
Een mobiel is in evenwicht wanneer het gewicht van tak 1 vermenigvuldigd
met de lengte van tak 1 gelijk is aan het gewicht van tak 2 vermenigvuldigd
met de lengte van tak 2, en wanneer deze conditie geldt voor alle sub-mobielen.
Definieer het predicaat @scheme[(balanced? m)], dat zegt of mobiel m gebalanceerd is.
@solution[@defs+int[((define (relative-weigth1 mobile)
                       (* (length1 mobile)
                          (mobile-weight (weight1 mobile))))
                     
                     (define (relative-weigth2 mobile)
                       (* (length2 mobile)
                          (mobile-weight (weight2 mobile))))
                     
                     (define (balanced? mobile)
                       (or (atom? mobile)
                           (and (= (relative-weigth1 mobile)
                                   (relative-weigth2 mobile))
                                (balanced? (weight1 mobile))
                                (balanced? (weight2 mobile))))))]]

