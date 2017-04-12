#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica januari 2010}
Onderstaande boomstructuur geeft een stukje weer van het organigram van de VUB.
De VUB organisatie kan onderverdeeld worden in een academisch stuk en in een
administratief stuk.
De adminstratie kan op haar beurt onderverdeeld worden in personeel, financiën, etc.
Elke dienst kan dan nog verder opgedeeld worden.
De academische organisatie bestaat uit het rectoraat en de faculteiten.
De faculteiten zijn bvb. Rechten, Economie en Wetenschappen.
(Er zijn nog andere faculteiten maar deze hebben we niet op het organigram aangeduid).

Elke faculteit heeft diverse bachelorprogramma's en masterprogramma's.
Let op, je oplossingen voor de vragen hieronder moeten algemeen zijn;
dus kunnen werken in situaties waar er nog een verder opdeling is van het rectoraat,
van de richtingen, etc.

@def+int[(define VUBOrganigram
           '(VUB (academisch (rectoraat)
                             (faculteiten
                              (rechten (bachelor(ba-rechten)
                                                (ba-criminologie)                                                   )
                                       (master(ma-rechten)
                                              (ma-criminologie)))
                              (economie)
                              (wetenschappen (bachelor (ba-wiskunde)
                                                       (ba-fysica)
                                                       (ba-cw))
                                             (master (ma-wiskunde)
                                                     (ma-fysica)
                                                     (ma-cw)))))
                 (administratief (personeel) (financien))))]

Maw de diepte van de boom ligt niet vast.

@image["bomen/images/vub.png" #:scale 0.175]

Veronderstel dat een procedure @scheme[(print-lijn aantalblanco tekst)] gegeven
is die als input een getal neemt en een tekst en die de tekst afprint op 1
lijn met het gegeven aantal blanco's ervoor.
@defs+int[((define (display-n n d)
             (if (> n 0)(begin (display d)(display-n (- n 1) d))))
           
           (define (print-lijn aantalblanco tekst)
             (display-n aantalblanco " ")
             (display tekst)
             (newline)))]

@solution[@interaction[(define (label organigram)  (car organigram))
                       (define (takken organigram) (cdr organigram))]]


@section{Print-vanaf}
Schrijf een procedure @scheme[(print-vanaf organigram label)] die
de organisatiestructuur weergeeft van een gegeven onderdeel
binnen een gegeven organigram en dit op een geïndenteerde manier.
Een voorbeeld (VUBOrganigram verwijst naar bovenstaande boomstructuur):

@solution[@defs+int[((define (organigram-member-in een-label organigrammen)
                       (if (null? organigrammen)
                           #f
                           (or (organigram-member een-label (car organigrammen))
                               (organigram-member-in een-label (cdr organigrammen)))))
                     
                     (define (organigram-member een-label organigram)
                       (if (eq? een-label (label organigram))
                           organigram
                           (organigram-member-in een-label (takken organigram))))
                     
                     (define (print organigram)
                       (define (print diepte organigram)
                         (print-lijn diepte (label organigram))
                         (for-each (lambda (organigram)
                                     (print (+ diepte 1) organigram))
                                   (takken organigram)))
                       (print 0 organigram))
                     
                     (define (print-vanaf organigram label)
                       (let ((res (organigram-member label organigram)))
                         (if res
                             (print res)
                             #f))))]]


@interaction[(print-vanaf VUBOrganigram 'rechten)]

@section{Print-tot}
Schrijf een procedure @scheme[(print-tot organigram niveau)] die
het organigram afprint tot op een bepaald niveau en dit terug geïndenteerd.

@solution[@def+int[(define (print-tot organigram niveau)
                     (define (print-tot organigram niveau max-niveau)
                       (cond ((<= niveau max-niveau)
                              (print-lijn niveau (label organigram))
                              (for-each 
                               (lambda (organigram)
                                 (print-tot organigram (+ niveau 1) max-niveau))
                               (takken organigram)))))
                     (print-tot organigram 0 niveau))]]


@interaction[(print-tot VUBOrganigram 2)]
