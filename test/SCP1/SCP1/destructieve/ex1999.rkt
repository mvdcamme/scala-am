#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica 2de zit 1999}
Reisbureau "Happy" en reisbureau "Tours" hebben besloten te fusioneren. Hiertoe moeten ze hun klantenbestanden samenvoegen. In beide gevallen wordt het klantenbestand voorgesteld door een geneste lijst, waarin van elke klant naam en adres wordt bijgehouden en die gesorteerd werd op naam. Bijvoorbeeld:

@defs+int[((define best1 '((ann (meiboomstraat 12 1820 Eppegem))
                           (bert (populierendreef 7 1050 Brussel))
                           (kurt (Mechelsesteenweg 50 1800 Vilvoorde))))
           
           (define best2 '((bert (populierendreef 7 1050 Brussel))
                           (jan (eikestraat 1 9000 Gent))
                           (sofie (boerendreef 5  2800 Mechelen)))))]

Deze twee bestanden moeten samengevoegd worden tot een nieuwe gesorteerde geneste lijst, maar doordat zulke bestanden vrij groot kunnen zijn, moet dit destructief gebeuren. Bovendien kan het gebeuren dat sommige personen klant waren bij beide bureaus en dus reeds in de twee bestanden zitten.

@section{Box-pointer diagram}
Teken een box-pointer diagram van hoe je het probleem gaat aanpakken op het gegeven voorbeeld.

@solution{@image["destructieve/images/bestanden.png" #:scale 0.15]}

@section{Samenvoegen van klantenbestanden}
Schrijf een procedure @scheme[(merge best1 best2)] die de twee bestanden best1 en best2 destructief samenvoegt tot een nieuwe gesorteerde lijst. Hou rekening met de dubbels.

@solution{@defs+int[((define (first-el best)
                       (if (not (null? best))
                           (caar best)
                           #f))
                     
                     (define (smaller? el1 el2)
                       (string<? (symbol->string el1) (symbol->string el2)))
                     
                     (define (same? el1 el2)
                       (equal? el1 el2))
                     
                     
                     (define (merge best1 best2)               
                       (define (merge-in curr1 curr2 prev)
                         (cond ((null? curr1) (set-cdr! prev curr2));lijsten zijn nooit tegelijkertijd leeg
                               ((null? curr2) (set-cdr! prev curr1))
                               ((same? (first-el curr1) (first-el curr2))
                                (set-cdr! prev curr1)
                                (merge-in (cdr curr1) (cdr curr2) curr1))
                               ((smaller? (first-el curr1) (first-el curr2))
                                (set-cdr! prev curr1)
                                (merge-in (cdr curr1) curr2 curr1))
                               (else
                                (set-cdr! prev curr2)
                                (merge-in curr1 (cdr curr2) curr2))))
                       
                       (let* ((result (if (smaller? (first-el best1) (first-el best2))
                                          best1
                                          best2))
                              (curr1 (if (eq? result best1) (cdr best1) best1))
                              (curr2 (if (eq? result best2) (cdr best2) best2)))
                         (merge-in curr1 curr2 result)
                         result))
                     )]}

@interaction[(merge best1 best2)]