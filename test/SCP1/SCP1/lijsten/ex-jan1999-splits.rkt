#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica januari 1999}

@section[#:tag "vraag1"]{Splits resultaten van een examen}
Op het secretariaat van de vakgroep computerwetenschappen wil men op het einde van de examenperiode op een eenvoudige wijze een overzicht bekomen van de behaalde resultaten. De professoren dienen resultaten in door ze voor te stellen als een lijst: @scheme['("algoritmen en datastructuren" 12 8 17 15 6 15)].

Het overzicht dat gevraagd wordt, splitst de puntenlijst ten opzichte van het gemiddelde. Er worden dus twee sublijsten gevormd met in de ene lijst de cijfers onder het gemiddelde en ind e andere lijst de cijfers boven het gemiddelde.

Schrijf een procedure @scheme[(splits resultaten)]. Voor het vak algoritmen en datastructuren zal het overzicht er als volgt uitzien: @scheme['("algoritmen en datastructuren" (12 8 6) (17 15 15))].

@solution{@def+int[(define (splits resultaten)
                     
                     (define (divide lst gem)
                       (if (null? lst) 
                           (list '() '())
                           (let* ((res (divide (cdr lst) gem))
                                  (smaller (car res))
                                  (higher (cadr res)))
                             (if (> (car lst) gem)
                                 (list smaller (cons (car lst) higher))
                                 (list (cons (car lst) smaller) higher)))))
                     
                     (define (calc-average lst)
                       (/ (apply + lst) (length lst)))
                     
                     (cons (car resultaten)
                           (divide (cdr resultaten) (calc-average (cdr resultaten)))))]}

@interaction[(splits '("algoritmen en datastructuren" 12 8 17 15 6 15))]


@section{Recursie/iteratie}
Levert je oplossing uit @secref{vraag1} een recursief of iteratief proces? Leg in je eigen woorden uit waarom je dit antwoord geeft (argumenteer bondig). Schrijf de andere versie voor @secref{vraag1}.

@solution{@def+int[(define (splits resultaten)
                     
                     (define (divide lst gem res)
                       (cond ((null? lst) res)
                             ((> (car lst) gem) 
                              (divide (cdr lst) 
                                      gem 
                                      (list (car res) 
                                            (append (cadr res) (list (car lst))))))
                             (else (divide (cdr lst) 
                                           gem 
                                           (list (append (car res) (list (car lst))) 
                                                 (cadr res))))))
                     
                     
                     (define (calc-average lst)
                       (/ (apply + lst) (length lst)))
                     
                     (cons (car resultaten)
                           (divide (cdr resultaten) 
                                   (calc-average (cdr resultaten)) 
                                   (list '() '()))))]}

@interaction[(splits '("algoritmen en datastructuren" 12 8 17 15 6 15))]


@section{Overzicht voor meerdere vakken}
Voor de deliberaties moet het secretariaat een overzicht geven voor alle vakken van het eerste jaar bachelor in de computerwetenschappen. De resultaten waarvan het secretariaat vertrekt zien er als volgt uit:
@def+int['("1ste BA CW"
           ("algoritmen en datastructuren" 12 8 17 15 6 15)
           ("structuur van computerprogramma's" 15 18 6 12 11)
           ; ...
           )]

Schrijf een procedure @scheme[(splits-alle lijst-resultaten)] die een overzicht berekent voor alle vakken. 
Merk op dat er voor de verschillende vakken niet noodzakelijk evenveel cijfers zijn.

@solution{@def+int[(define (splits-alle lijst-resultaten)
                     (cons (car lijst-resultaten) 
                           (map splits (cdr lijst-resultaten))))]}

@interaction[(splits-alle '("1ste BA CW"
                            ("algoritmen en datastructuren" 12 8 17 15 6 15)
                            ("structuur van computerprogramma's" 15 18 6 12 11)))]
