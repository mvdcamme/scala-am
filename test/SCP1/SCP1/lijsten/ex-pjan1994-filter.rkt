#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica Partieel januari 1994}

@section{Filteren van elementen in een lijst}
Schrijf een hogere-orde procedure @scheme[filter] die een gegeven predicaat loslaat op alle elementen van een lijst. Ze geeft een nieuwe lijst terug met alle elementen waarvoor het predicaat waar is.

@solution{@defs+int[(;Een mogelijke oplossing is:
                     (define (filter pred lst)
                       (apply append 
                              (map 
                               (lambda (x) (if (pred x) (list x) '())) 
                               lst)))
                     
                     ;Recursief:
                     (define (filter pred lst)
                       (cond ((null? lst) '())
                             ((pred (car lst)) 
                              (cons (car lst) (filter pred (cdr lst))))
                             (else (filter pred (cdr lst))))))]}

@interaction[(filter (lambda (x) (odd? x)) '(1 2 3 4 5 6 7 8))]


@section{Lijst van alle niet-getallen}
Gebruik de bovenstaande @scheme[filter] en het standaard-predicaat @scheme[number?] (@scheme[#t] als de parameter een getal is, @scheme[#f] in alle andere gevallen) om een lijst te maken met alle NIET-getalen van @scheme['(1 a b 33 7 c)] zodat het resultaat @scheme['(a b c)] geproduceerd wordt.

@solution{@def+int[(define (filter-not-nbrs lst)
                     (filter (lambda (x) (not (number? x))) lst))]}

@interaction[(filter-not-nbrs '(1 a b 33 7 c))]


@section{Deep-filter}
Herwerk @scheme[filter] tot een nieuwe hogere-orde procedure @scheme[deep-filter] die werkt op een geneste lijst. Ze maakt een nieuwe lijst met dezelfde structuur als de gegeven lijst, maar de elementen waarvoor het predicaat vals is worden vervangen door een lege lijst, de elementen waarvoor het predicaat waar is blijven staan.

@solution{@def+int[(define (deep-filter pred lst)
                     (cond ((null? lst) '())
                           ((pair? (car lst)) 
                            (cons (deep-filter pred (car lst))
                                  (deep-filter pred (cdr lst))))
                           ((pred (car lst)) 
                            (cons (car lst) (deep-filter pred (cdr lst))))
                           (else (cons '() (deep-filter pred (cdr lst))))))]}

@interaction[(deep-filter (lambda (x) (odd? x)) 
                          '(1 2 3 (4) (5 (6 7) 8) (9) (10 (11) 12) 13 14))]

@interaction[(deep-filter number? '(1 (a b (1)) 5))]


@section{Deep-filter zonder lege lijsten}
Tijdens het uittesten merken we op dat bovenstaande @scheme[deep-filter] teveel lege lijsten produceert. Als we bijvoorbeeld @scheme[deep-filter] oproepen met parameters @scheme[number?] en @scheme['(1 (a b (1)) 5)] bekomen we @scheme[(1 (() () (1)) 5)].
Herwerk daarom @scheme[deep-filter] tot een @scheme[deep-filter2] zodat lege lijsten waar mogelijk verwijderd worden. Als we @scheme[deep-filter2] oproepen met bovenstaande parameters moeten we @scheme[(1 ((1)) 5)] bekomen.

@solution{@def+int[(define (deep-filter2 pred lst)
                     (cond ((null? lst) '())
                           ((pair? (car lst)) 
                            (cons (deep-filter2 pred (car lst))
                                  (deep-filter2 pred (cdr lst))))
                           ((pred (car lst)) 
                            (cons (car lst) (deep-filter2 pred (cdr lst))))
                           (else (deep-filter2 pred (cdr lst)))))]}

@interaction[(deep-filter2 number? '(1 (a b (1)) 5))]



