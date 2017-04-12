#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica augustus 2009}
Een classificatieboom is een speciaal soort boom die een aantal soorten (van dieren, planten, ...) classificeert volgens een bepaalde hiërarchie. Elke knoop in de boom beschrijft een bepaalde soort, met zijn specifieke eigenschappen. Bijvoorbeeld een "dier" is een soort die "kan ademen" en "kan bewegen". Alle deelsoorten van een bepaalde soort hebben dezelfde eigenschappen als die soort, maar kunnen eventueel ook nog een aantal extra eigenschappen bezitten. Bijvoorbeeld een "landdier" is een speciaal geval van een "dier" dat een "huid heeft", "poten heeft" en "kan lopen", maar natuurlijk heeft het nog steeds de eigenschappen "kan ademen" en "kan bewegen", aangezien het een "dier" is. 

Merk op dat we in de knopen die de deelsoorten beschrijven alleen de bijkomende eigenschappen opnemen. Hieronder vind je een uitgewerkt voorbeeld van zo'n classificatieboom:

@image["bomen/images/klassen.png" #:scale 0.70]

@section{Abstract Data Type}
Schrijf een ADT om een knoop van zo'n classificatieboom voor te stellen. Dit mag een simpel ADT zijn waarmee je de naam van een knoop en een lijst van zijn eigenschappen kan bijhouden. Schrijf ook een ADT om de classificatieboom zelf voor te stellen. Dit mag een simpel ADT zijn waarmee je de vader-knoop en een lijst van al zijn deelbomen kan bijhouden.

@solution{@defs+int[("ADT voor knoop"
                     (define (maak-dier naam eigenschappen)
                       (list naam eigenschappen))
                     
                     (define (naam dier) (car dier))
                     (define (eigenschappen dier) (cadr dier))
                     
                     (define (dier? dier)
                       (and (pair? dier)
                            (atom? (naam dier))
                            (pair? (eigenschappen dier))))
                     
                     "ADT voor boom"
                     (define (maak-boom knoop deelbomen)
                       (list knoop deelbomen))
                     
                     (define (knoop boom) (car boom)) 
                     (define (deelbomen boom) (cadr boom))
                     (define (leeg? boom) (null? boom))  
                     (define (knoop? boom) (dier? boom))
                     
                     ; Voorbeeld: onze classificatieboom
                     (define classificatieboom 
                       (maak-boom (maak-dier 'dier '(kan-ademen kan-bewegen))
                                  (list 
                                   (maak-boom 
                                    (maak-dier 'vis 
                                               '(kan-zwemmen heeft-schubben heeft-vinnen))
                                    (list 
                                     (maak-dier 'ballonvis 
                                                '(kan-zwellen is-geel)) ))
                                   (maak-boom 
                                    (maak-dier 'landdier 
                                               '(heeft-huid kan-lopen heeft-poten))
                                    (list (maak-dier 'olifant 
                                                     '(is-groot)) ))
                                   (maak-boom 
                                    (maak-dier 'vogel 
                                               '(kan-vliegen heeft-vleugels heeft-veren))
                                    (list 
                                     (maak-dier 'kanarie 
                                                '(kan-zingen is-geel))
                                     (maak-dier 'arend 
                                                '(is-groot)) ))))))]}


@section{Geef alle soorten}
Schrijf een functie @scheme[(all-kinds cboom)] die een classificatieboom @scheme[cboom] als invoer neemt en alle soorten uit die classificatieboom in een lijstje teruggeeft. 

@solution{@def+int[(define (all-kinds boom)
                     (cond ((leeg? boom) '())
                           ((dier? boom) (list (naam boom)))
                           ((dier? (knoop boom)) 
                            (append (list (naam (knoop boom))) 
                                    (all-kinds-in (deelbomen boom))))
                           (else (all-kinds-in (deelbomen boom)))))
                   
                   (define (all-kinds-in lst)
                     (if (null? lst)
                         '()
                         (append (all-kinds (car lst))
                                 (all-kinds-in (cdr lst)))))]}

@interaction[(all-kinds classificatieboom)]


@section{Verifieer eigenschap}
Schrijf een predicaat @scheme[(ask? cboom soort eigenschap)] dat
controleert of een bepaalde soort uit de classificatieboom een bepaalde 
eigenschap bezit. Hou er rekening mee dat alle eigenschappen die gelden 
voor een bepaalde soort in de boom, automatisch ook gelden voor al zijn 
deelsoorten, ook al staan ze daar niet expliciet opnieuw opgesomd. Zo 
heeft een ballonvis bijvoorbeeld ook vinnen, aangezien alle vissen vinnen
hebben.

@solution{@defs+int[((define (geef-eigenschappen boom soort)
                       (define (geef-eig boom eig)   
                         (cond ((dier? boom)
                                (if (eq? (naam boom) soort)
                                    (append eig 
                                            (list (eigenschappen boom)))
                                    #f))
                               ((and (dier? (knoop boom)) 
                                     (eq? (naam (knoop boom)) soort))
                                (append eig (eigenschappen (knoop boom))))
                               (else (geef-eig-in (deelbomen boom) 
                                                  (append eig 
                                                          (eigenschappen (knoop boom)))))))
                       
                       (define (geef-eig-in lst eig)
                         (cond ((null? lst) #f)
                               (else (or (geef-eig (car lst) eig)
                                         (geef-eig-in (cdr lst) eig)))))
                       (geef-eig boom '()))
                     
                     (define (ask? boom soort eig)
                       (let ((eigenschappen (geef-eigenschappen boom soort)))
                         (pair? (memq eig eigenschappen)))))]}

@interaction[(ask? classificatieboom 'landdier  'kan-lopen)]
@interaction[(ask? classificatieboom 'ballonvis 'heeft-vinnen)]
@interaction[(ask? classificatieboom 'olifant   'kan-vliegen)]


