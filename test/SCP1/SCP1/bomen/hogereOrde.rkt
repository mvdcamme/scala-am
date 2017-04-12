#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Hogere Orde}
@section{Deep-combine}
Definieer de functie @scheme[(deep-combine combiner null-value l)],
die alle atomen van een (eventueel geneste) lijst combineert.
Hou rekening met dotted-pairs.

@solution[@def+int[(define (deep-combine combiner null-value l)
                     (cond ((null? l) null-value)
                           ((atom? l) l)
                           (else (combiner (deep-combine combiner 
                                                         null-value 
                                                         (car l))
                                           (deep-combine combiner 
                                                         null-value 
                                                         (cdr l))))))]]


@interaction[(deep-combine + 0 '((((1 2) (3 4)) ((5 6) (7 8))) 9))]

@section{Deep-map}
Definieer de functie @scheme[(deep-map f l)],
die als resultaat een lijst teruggeeft met dezelfde structuur
als @scheme[l], maar waarin alle atomen vervangen zijn door het
resultaat van @scheme[f] op het atoom.
Hou rekening met dotted pairs.

@solution[@def+int[(define (deep-map f l)
                     (cond
                       ((null? l) '())
                       ((atom? l) (f l))
                       (else (cons (deep-map f (car l))
                                   (deep-map f (cdr l))))))]]


@interaction[(deep-map (lambda (x) (* x x))
                       '((((1 . 2) (3 4)) ((5 6) (7 8))) . 9))]

@section{Deep-change}
Definieer @scheme[(deep-change e1 e2 l)], aan de hand van
@scheme[deep-map] en/of @scheme[deep-combine],
die een lijst terug met dezelfde structuur als @scheme[l],
maar alle atomen @scheme[e1] verandert in @scheme[e2].

@solution[@def+int[(define (deep-change e1 e2 l)
                     (deep-map (lambda (e) (if (eq? e e1) e2 e)) l))]]



@section{Deep-atom-member?}
Definieer @scheme[(deep-atom-member? e l)], aan de hand van
@scheme[deep-map] en/of @scheme[deep-combine],
die kijkt of @scheme[e] (een atoom) ergens in een geneste 
lijst voorkomt.

@solution[@def+int[(define (deep-atom-member? e l)
                     (deep-combine (lambda (car cdr) (or car cdr))
                                   #f
                                   (deep-map (lambda (x) (eq? e x)) l)))]]


@section{Count-atoms}
Definieer @scheme[(count-atoms l)], aan de hand van
@scheme[deep-map] en/of @scheme[deep-combine],
die het aantal atomen in een (evt. geneste) lijst telt.

@solution[@def+int[(define (count-atoms l)
                     (deep-combine + 0 (deep-map (lambda (x) 1) l)))]]

