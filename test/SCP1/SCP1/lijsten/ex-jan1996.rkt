#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica Partieel januari 1996}
Veronderstel dat we woorden voorstellen als lijsten van symbolen. Bijvoorbeeld het woordje "computer" wordt voorgesteld door de lijst @scheme[(c o m p u t e r)]. Veronderstel bovendien dat je beschikt over een predicaat @scheme[klinker?] dat @scheme[#t] geeft indien een symbool een klinker voorstelt, en @scheme[#f] in alle andere gevallen.

@section{P-taal}
Schrijf een procedure @scheme[P-taal] die als invoer zo'n woord neemt, en dit "vertaalt" in de "P-taal" door elke klinker te vervangen door diezelfde klinker, gevolgd door een @scheme['p], en dan nog eens die klinker.

@solution{@defs+int[((define (klinker? x)
                       (or (eq? x 'a) (eq? x 'e) (eq? x 'i)
                           (eq? x 'o) (eq? x 'u)))
                     
                     (define (P-taal woord)
                     (if (null? woord) '()
                         (append (let ((letter (car woord)))
                                   (if (klinker? letter)
                                       (list letter 'P letter)
                                       (list letter)))
                                 (P-taal (cdr woord))))))]}

@interaction[(P-taal '(c o m p u t e r))
             (P-taal '(w e t e n s c h a p p e n))]


@section{Recursie/iteratie}
Genereert deze procedure een recursief of iteratief proces? Herschrijf de procedure op een zodanige manier (indien mogelijk) dat ze een proces van het andere type genereert. Welke van de twee is nu het snelst?

@solution{De vorige was recursief. Performantie: O(n) want de lijst 1 keer element per element doorlopen, en telkens een lijstje van lengte 1 of 3 eraan appenden.}

@solution{@def+int[(define (P-taal woord)
                     (define (iter rest result)
                       (if (null? rest)
                           (reverse result)
                           (iter (cdr rest) (append (let ((letter (car rest)))
                                                      (if (klinker? letter)
                                                          (list letter 'P letter)
                                                          (list letter)))
                                                    result))))
                     (iter woord '()))]
           
           Deze versie is iets minder performant, maar is ook O(n). Het enige verschil met de vorige is dat hier op het einde nog moet gereversed worden en bij de vorige niet. Dus iets trager maar zelfde orde. Opmerking: de iteratieve is natuurlijk wel beter wat betreft geheugengebruik.
           
           Ziehier een "slechte" iteratieve versie die O(n*n) performantie heeft:
           
           @def+int[(define (P-taal woord)
                      (define (iter rest result)
                        (if (null? rest)
                            result
                            (iter (cdr rest) (append
                                              result
                                              (let ((letter (car rest)))
                                                (if (klinker? letter)
                                                    (list letter 'P letter)
                                                    (list letter)))))))
                      (iter woord '()))]
           
           De reden dat deze iteratieve versie veel slechter is dan de vorige iteratieve  versie is omdat hier telkens de append gebeurt van een steeds langer wordende lijst aan een lijstje van maximaal lengte 3. De performantie van de append is echter rechtevenredig met de lengte van de eerste lijst. Dus de globale snelheidsperformantie is O(n*n) door een toepassing van een formule a la 1+2+3+4+...+n = n(n-1)/2}
           