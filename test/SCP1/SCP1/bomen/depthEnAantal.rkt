#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Diepte en bladeren van een boom}
We weten dat we een boom kunnen voorstellen
d.m.v. een diepgeneste lijst.

@section{Diepte van een boom}
Schrijf een procedure depth die de
nesting-diepte van een boom berekent.
M.a.w. hoeveel haakjes staan er rond het
meest geneste element van de boom?
@solution[@def+int[(define (depth tree)
                     (cond ((null? tree) 0)
                           ((atom? tree) 0)
                           (else (max (+ 1 (depth (car tree)))
                                      (depth (cdr tree))))))]]


@interaction[(depth '((1 2) ((3 4) 5) (6 7)))]

@section{Aantal elementen van een boom}
Schrijf een Scheme-procedure die het aantal
elementen (= bladeren) in zo'n boom telt.

@solution[@def+int[(define (leaf-count tree)
                     (cond
                       ((null? tree) 0)
                       ((atom? tree) 1)
                       (else (+ (leaf-count (car tree))
                                (leaf-count (cdr tree))))))]]


@interaction[(leaf-count '((1 2) ((3 4) 5) (6 7)))]


@section{Diepte en aantal elementen van een boom}
Schrijf een procedure die (a) en (b) combineert,
maar zo efficiënt mogelijk.
D.w.z. de boom mag maar 1 enkele keer
volledig doorlopen worden.
@solution[@def+int[(define (depth-and-leaf-count tree)
                     (define make-res cons)
                     (define depth car)
                     (define leaf-count cdr)
                     
                     (cond
                       ((null? tree) (make-res 0 0))
                       ((atom? tree) (make-res 0 1))
                       (else (let ((res-car (depth-and-leaf-count (car tree)))
                                   (res-cdr (depth-and-leaf-count (cdr tree))))
                               (make-res (max (+ 1 (depth res-car))
                                              (depth res-cdr))
                                         (+ (leaf-count res-car) 
                                            (leaf-count res-cdr)))))))]]


@interaction[(depth-and-leaf-count '((1 2) ((3 4) 5) (6 7)))]


