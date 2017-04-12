#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Sommatie van elementen in lijsten}
De procedure @scheme[(sum-lists l1 l2)] heeft als argumenten 2 lijsten met getallen, en geeft een lijst terug met de sommen van de overeenkomstige elementen van de input-lijsten.

Merk op dat de twee input-lijsten niet dezelfde lengte hoeven te hebben.
In dat geval worden de resterende elementen van de langste lijst gewoon overgenomen.

@section{Recursieve versie}
Schrijf een recursieve versie van @scheme[sum-lists].

@solution{@def+int[(define (sum-lists l1 l2)
                     (cond ((null? l1) l2)
                           ((null? l2) l1)
                           (else (cons (+ (car l1) (car l2)) 
                                       (sum-lists (cdr l1) (cdr l2))))))]}

@interaction[(sum-lists '(1 2 3 4) '(5 6 7))
             (sum-lists '(1 2 3 4) '())
             (sum-lists '() '(1 2 3))]

@section{Iteratieve versie}
Schrijf een iteratieve versie van @scheme[sum-lists].

@solution{@def+int[(define (sum-lists l1 l2)
                     (define (iter lst1 lst2 res)
                       (cond ((and (null? lst1) (null? lst2)) (reverse res))
                             ((null? lst1) (iter lst1 (cdr lst2) (cons (car lst2) res)))
                             ((null? lst2) (iter (cdr lst1) lst2 (cons (car lst1) res)))
                             (else (iter (cdr lst1) 
                                         (cdr lst2) 
                                         (cons (+ (car lst1) (car lst2)) res)))))
                     (iter l1 l2 '()))]}

@interaction[(sum-lists '(1 2 3 4) '(5 6 7))
             (sum-lists '(1 2 3 4) '())
             (sum-lists '() '(1 2 3))]



