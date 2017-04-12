#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Verander element in een lijst}
@section{Change}
Schrijf de functie @scheme[(change e1 e2 l)] die een lijst teruggeeft, gelijk aan lijst @scheme[l], maar waarin alle elementen gelijk aan @scheme[e1] vervangen zijn door @scheme[e2]. Gebruik @scheme[(eq? e1 e2)] om twee elementen te vergelijken. De procedure mag een recursief proces genereren. 

@solution{@def+int[(define (change e1 e2 l)
                     (cond ((null? l) l)
                           ((eq? (car l) e1) (cons e2 (change e1 e2 (cdr l))))
                           (else (cons (car l) (change e1 e2 (cdr l))))))]}

@interaction[(change 1 999 '(1 2 1 3 1 4 1 5 1))
             (change 1 999 '(2 3 4 5 6 7))
             (change 1 999 '(1))
             (change 1 999 '())]

           
           
@section{Change d.m.v. map}
Implementeer de procedure @scheme[change] door middel van @scheme[map].

@solution{@def+int[(define (change e1 e2 l)
                     (map (lambda (x) (if (eq? x e1) e2 x)) l))]}

@interaction[(change 1 999 '(1 2 1 3 1 4 1 5 1))
             (change 1 999 '(2 3 4 5 6 7))
             (change 1 999 '(1))
             (change 1 999 '())]