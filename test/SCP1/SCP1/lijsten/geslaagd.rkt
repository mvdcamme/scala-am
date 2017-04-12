#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica januari 2005}
Gegeven zijn 2 vlakke lijsten van dezelfde lengte.
De eerste lijst is een alfabetische namenlijst,
die alle namen bevat van de studenten die het vak Structuur I volgen. De tweede lijst is een puntenlijst, die uitsluitend uit getallen bestaat. Een getal op een bepaalde positie uit deze lijst is het cijfer behaald voor Structuur I door de student die op dezelfde positie in de namenlijst staat.

@section{Geslaagd}
Schrijf een functie @scheme[geslaagd] in Scheme die
met behulp van deze twee lijsten, een lijst construeert
met alle namen van die studenten die meer dan 10
behaalden op het vak Structuur I.
@solution{@def+int[(define (geslaagd namen punten)
                     (if (null? namen)
                         '()
                         (let ((res (geslaagd (cdr namen)(cdr punten))))
                           (if (> (car punten) 10)
                               (cons (car namen) res)
                               res))))]}
Bijvoorbeeld:
@interaction[(geslaagd '(wendy dirk kris jan eef) '(12 7 13 9 18))]

@section{Process}
Levert je oplossing een recursief of
een iteratie proces? Leg in je eigen woorden
uit waarom je dit antwoord geeft. Schrijft de 
andere versie.
@solution{@def+int[(define (geslaagd namen punten)
                     (define (geslaagd namen punten res)
                       (cond
                         ((null? namen)(reverse res))
                         ((> (car punten) 10)(geslaagd (cdr namen) (cdr punten) 
                                                       (cons (car namen) res)))
                         (else (geslaagd (cdr namen) (cdr punten) res))))
                     (geslaagd namen punten '()))
                   (geslaagd '(wendy dirk kris jan eef) '(12 7 13 9 18))]}



Veronderstel dat de puntenlijst uitgebreid wordt naar een lijst van lijstjes.
Elk deellijstje op een bepaalde positie bevat nu de cijfers voor respectievelijk
de vakken Structuur I, AlgoData I, Grondslagen I en Discrete I.
Deze cijfers behoren tot de student die je in de namenlijst vindt op dezelfde positie.

@section{Algemener}
Schrijf een algemene functie @scheme{show} die gegeven de namenlijst,
de uitgebreide puntenlijst en een test,
een lijst construeert van alle studenten die aan deze test voldoen.
@solution{@def+int[(define (show namen punten test?)
                     (if (null? namen)
                         '()
                         (let ((res (show (cdr namen) (cdr punten) test?)))
                           (if (test? (car punten))
                               (cons (car namen) res)
                               res))))]}

@section{Hergebruik}
Gebruik je oplossing  voor @scheme[show] om een functie @scheme[one] te schrijven die
alle namen teruggeeft van die studenten die slechts 1 buiscijfer (< 10) behaalden.
Bijvoorbeeld:
@solution{@def+int[(define (one namen punten)
                     (define (één-buis? punten)
                       (if (null? punten)
                           #f
                           (let ((punt (car punten))
                                 (rest (cdr punten)))
                             (if (< punt 10)
                                 (geen-buis? rest)
                                 (één-buis? rest)))))
                     
                     
                     (define (geen-buis? punten)
                       (if (null? punten)
                           #t
                           (let ((punt (car punten))
                                 (rest (cdr punten)))
                             (if (< punt 10)
                                 #f
                                 (geen-buis? rest)))))
                     
                     (show namen punten één-buis?))]}
@interaction[(one '(wendy dirk kris jan eef)
                  '((12 13 15 18) (7 10 14 17) (13 8 7 11)
                                  (9 12 11 10) (18 14 17 19)))]