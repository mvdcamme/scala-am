#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica 2eZit 1995}
Beschouw het organigram van een bedrijf.
De meest voor de hand liggende representatie van zo'n organigram is de boomstructuur.
Hieronder vind je een voorbeeld van zo een bedrijf.

@image["bomen/images/bedrijf.png" #:scale 0.15]

Bovenstaande boomstructuur kunnen we in Scheme gemakkelijk
voorstellen d.m.v. geneste lijsten:
@def+int[(define organigram
           '(directeur
             (hoofd-verkoop (verkoopsleider-vlaanderen)
                            (verkoopsleider-brussel))
             (hoofd-productie (hoofd-inkoop (bediende1)
                                            (bediende2)
                                            (bediende3))
                              (hoofd-fakturen))
             (hoofd-administratie (hoofd-personeel)
                                  (hoofd-boekhouding))))]

@solution[@interaction[(define (baas organigram) (car organigram))
                       (define (sub-organigrammen organigram) (cdr organigram))]]


@section{Hiërarchisch?}
Schrijf een predicaat @scheme[(hierarchisch? p1 p2 organigram)],
dat nagaat of er een hiërarchische relatie bestaat tussen 2 personeelsleden.
(D.w.z. liggen ze op eenzelfde pad in de boom?)

@solution[@def+int[(define (hierarchisch? p1 p2 organigram)
                     (define (hierarchisch?-in path organigrammen)
                       (if (null? organigrammen)
                           #f
                           (or (hierarchisch? path (car organigrammen))
                               (hierarchisch?-in path (cdr organigrammen)))))
                     
                     (define (hierarchisch? path organigram)
                       (cond
                         ((and (eq? p1 (baas organigram)) (member p2 path)) #t)
                         ((and (eq? p2 (baas organigram)) (member p1 path)) #t)
                         (else (hierarchisch?-in (cons (baas organigram) path)
                                                 (sub-organigrammen organigram)))))
                     (hierarchisch? '() organigram))]]



@interaction[(hierarchisch? 'directeur 'verkoopsleider-brussel organigram)
             (hierarchisch? 'bediende1 'hoofd-productie organigram)
             (hierarchisch? 'hoofd-personeel 'bediende3 organigram)]

@section{Collega's}
Eén van de personeelsleden krijgt binnenkort een dochtertje.
Volgens de bedrijfstraditie moet hij daarom doopsuiker sturen naar al de
personeelsleden waarvan hij het (rechtstreekse of onrechtstreekse) hoofd is,
evenals naar al zijn oversten.
Schrijf een procedure @scheme[(collegas p organigram)] die een lijstje met al deze personen opstelt.

@solution[@def+int[(define (collegas p organigram)
                     (define (collegas-in oversten organigrammen)
                       (if (null? organigrammen)
                           #f
                           (or (collegas oversten (car organigrammen))
                               (collegas-in oversten (cdr organigrammen)))))
                     
                     (define (werknemers-in organigrammen)
                       (if (null? organigrammen)
                           '()
                           (append (werknemers (car organigrammen))
                                   (werknemers-in (cdr organigrammen)))))
                     
                     (define (werknemers organigram)
                       (cons (baas organigram)
                             (werknemers-in (sub-organigrammen organigram))))
                     
                     (define (collegas oversten organigram)
                       (if (eq? p (baas organigram))
                           (append oversten 
                                   (werknemers-in (sub-organigrammen organigram)))
                           (collegas-in (cons (baas organigram) oversten)
                                        (sub-organigrammen organigram))))
                   (collegas '() organigram))]]


@interaction[(collegas 'hoofd-inkoop organigram)]
