#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica tweede zit 1999: Boom vraag}
Naar aanleiding van het komende millenniumfeest wordt door de overheid een steekproef gedaan naar de kwaliteit van het vuurwerk geleverd door verschillende leveranciers. Een computermodel
van een afgevuurd vuurwerkstuk werd opgesteld in Scheme.
Het ziet er als volgt uit:

@image["bomen/images/vuurwerk.png" #:scale 0.40]

Een vuurwerk spat uit elkaar in opeenvolgende knallen.
Elke knal heeft een kleur en een aantal vertakkingen in die kleur.
Een vertakking kan, indien er nog genoeg energie is opnieuw knallen en
dit eventueel in een andere kleur.
Dit geeft volgende lijstvoorstelling voor bovenstaand voorbeeld:

@def+int[(define mijn-vuurwerk '(groen(( blauw ( X ( blauw ( X X )) X X ))
                                       (rood ((groen(X X))X))
                                       X
                                       ( geel ( X X )))))]

Een vertakking die eindigt zonder of met te weinig energie
zal niet meer verder knallen.
Dit wordt in het model aangeduid met een @scheme['X].

We kunnen nu enkele testen implementeren:
@solution[@defs+int[((define (kleur vuurwerk) (car vuurwerk))
                     (define (takken vuurwerk) (cadr vuurwerk))
                     (define (low-energy? vuurwerk) (eq? vuurwerk 'X)))]]

@interaction[(kleur mijn-vuurwerk)
             (takken mijn-vuurwerk)
             (low-energy? mijn-vuurwerk)
             (low-energy? 'X)]

@section{Tel-knallen}
Schrijf een procedure @scheme[(tel-knallen vuurwerk)] die
het totaal aantal knallen van het gegeven vuurwerkstuk telt.
@solution[@def+int[(define (tel-knallen vuurwerk)
                     (cond ((null? vuurwerk) 0)
                           ((low-energy? vuurwerk) 0)
                           ((atom? vuurwerk) 1)
                           (else (+ (tel-knallen (car vuurwerk))
                                    (tel-knallen (cdr vuurwerk))))))]]

@interaction[(tel-knallen mijn-vuurwerk)]

@section{Tel-einde}
Schrijf een procedure @scheme[(tel-einde vuurwerk kleur)] die
het aantal vertakkingen van een bepaalde kleur telt,
waarvan de energie te laag is om verder te knallen.
@solution[@defs+int[((define (tel-low-energies v)
                       (cond ((null? v) 0)
                             ((low-energy? v) 1)
                             ((atom? v) 0)
                             (else (+ (tel-low-energies (car v)) 
                                      (tel-low-energies (cdr v))))))
                     
                     (define (tel-einde-in takken een-kleur)
                       (cond ((null? takken) 0)
                             ((low-energy? (car takken)) 0)
                             (else (+ (tel-einde (car takken) een-kleur)
                                      (tel-einde-in (cdr takken) een-kleur)))))
                     
                     (define (tel-einde vuurwerk een-kleur)
                       (if (eq? (kleur vuurwerk) een-kleur)
                           (tel-low-energies (takken vuurwerk))
                           (tel-einde-in (takken vuurwerk) een-kleur))))]]


@interaction[(tel-einde mijn-vuurwerk 'blauw)]

@section{Ster?}
Schrijf een procedure @scheme[(ster? vuurwerk)] die nagaat of een vuurwerkstuk
voldoet aan volgende eis: na de eerste knal heeft elke vertakking nog
voldoende energie om zelf ook te knallen. Bovendien moeten ze dit doen met elk
evenveel vertakkingen.
@solution[@def+int[(define (ster? vuurwerk)
                     (not (member 'X (takken vuurwerk))))]]

@interaction[(ster? mijn-vuurwerk)]