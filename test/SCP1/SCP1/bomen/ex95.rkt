#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica Partieel januari 1995}
Het laboratorium voor biotechnologie aan de VUB probeert een soort
hybride appelboom te ontwikkelen waarop verschillende soorten appels
kunnen groeien.
Elke tak van zo'n boom kan vertakken in een nieuwe tak,
maar er kunnen ook 1 of meerdere bladeren en/of appels aan hangen.
Hieronder vind je een voorbeeld van zo'n boom.

@image["bomen/images/appelboom2.png" #:scale 0.14]

Om de biologische eigenschappen van deze nieuwe boomsoort te voorspellen
contacteren de onderzoekers het departement informatica om er een
computermodel voor te ontwikkelen.
In dit model zal de appelboom worden voorgesteld als een diepgeneste lijst.
De bovenstaande boom ziet er in het model bijvoorbeeld als volgt uit:

@def+int[(define boom
           '((blad (appel . golden))
             (blad (appel . granny))
             (((appel . golden) blad) blad (appel . cox))))]

@section{Tel bladeren}
Schrijf een procedure @scheme[(leafs boom)] die het aantal bladeren in zo'n
appelboom kan tellen.
@solution[@defs+int[((define (blad? boom)
                       (eq? boom 'blad))
                     
                     (define (appel? boom)
                       (and (pair? boom) (eq? (car boom) 'appel)))
                     
                     (define (type appel) (cdr appel))
                     
                     (define (leafs boom)
                       (cond  ((null? boom) 0)
                              ((blad? boom) 1)
                              ((appel? boom) 0)
                              (else (+ (leafs (car boom))
                                       (leafs (cdr boom)))))))]]

@interaction[(leafs boom)]

@section{Zoek alle appels}
Schrijf een procedure @scheme[(all-apples boom)] die een lijstje maakt van alle appels die
in een gegeven appelboom voorkomen.
@solution[@def+int[(define (all-apples boom)
                     (cond ((null? boom) '())
                           ((blad? boom) '())
                           ((appel? boom) (list (type boom)))
                           (else (append (all-apples (car boom))
                                         (all-apples (cdr boom))))))]]


@interaction[(all-apples boom)]

@section{Geef de verschillende soorten appels}
Schrijf een procedure @scheme[(apple-types boom)] die een lijstje maakt van alle soorten
appels die in een gegeven appelboom voorkomen.
Het verschil met @scheme[(all-apples boom)] zit hem in het feit dat je nu geen dubbels
in de resultaatlijst wil.
@solution[@defs+int[((define (conditional-append l1 l2)
                       (cond
                         ((null? l1) l2)
                         ((member (car l1) l2)(conditional-append (cdr l1) l2))
                         (else (cons (car l1)(conditional-append (cdr l1) l2)))))
                     
                     (define (apple-types boom)
                       (cond ((null? boom) '())
                             ((blad? boom) '())
                             ((appel? boom) (list (type boom)))
                             (else (conditional-append (apple-types (car boom))
                                                       (apple-types (cdr boom)))))))]]


@interaction[(apple-types boom)]

@section{Procedure om de boom te bewerken}
Schrijf een hogere-orde procedure
@scheme[(bewerk-boom boom doe-blad doe-appel combiner init)]
die je in het algemeen kan gebruiken om een appelboom te bewerken.
@solution[@def+int[(define (bewerk-boom boom doe-blad doe-appel combiner init)
                     (cond
                       ((null? boom) init)
                       ((blad? boom) (doe-blad boom))
                       ((appel? boom) (doe-appel boom))
                       (else (combiner
                              (bewerk-boom (car boom) doe-blad doe-appel combiner init)
                              (bewerk-boom (cdr boom) doe-blad doe-appel combiner init)))))]]




@section{Tel bladeren (hogere orde)}
Herschrijf @scheme[(leafs boom)] d.m.v. @scheme[bewerk-boom].
@solution[@def+int[(define (leafs-dmv-bewerk boom)
                     (bewerk-boom boom 
                                  (lambda (blad) 1) 
                                  (lambda (appel) 0) 
                                  + 
                                  0)) ]]


@interaction[(leafs-dmv-bewerk boom)]

@section{Geef alle appels (hogere orde)}
Herschrijf @scheme[all-apples] d.m.v. @scheme[bewerk-boom].
@solution[@def+int[(define (all-apples-dmv-bewerk boom)
                     (bewerk-boom boom 
                                  (lambda(blad) '()) 
                                  (lambda(appel) (list (type appel))) 
                                  append 
                                  '()))]]


@interaction[(all-apples-dmv-bewerk boom)]

@section{Geef de verschillende soorten appels (hogere orde)}
Herschrijf @scheme[apple-types] d.m.v. @scheme[bewerk-boom].
@solution[@def+int[(define (apple-types-dmv-bewerk boom) 
                     (bewerk-boom boom 
                                  (lambda(blad) '()) 
                                  (lambda(appel) (list(type  appel))) 
                                  conditional-append 
                                  '()))]]


@interaction[(apple-types-dmv-bewerk boom)]
