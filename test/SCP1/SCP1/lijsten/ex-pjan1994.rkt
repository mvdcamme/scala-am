#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica Partieel januari 1994}
@section{Kopie van een lijst, zonder elementen in een interval}
Schrijf een lineair recursieve procedure @scheme[all-but-interval] die een kopie neemt van een geordende lijst getallen, uitgezonderd die getallen binnen een bepaald interval. Je mag ervan uitgaan dat elk getal in de lijst strikt groter is dan zijn voorganger (er komen geen dubbels voor in de lijst). Er worden twee parameters doorgegeven om het interval aan te duiden, en de eerste is kleiner of gelijk aan de tweede.

@solution{@def+int[(define (all-but-interval l start stop)
                     (cond ((null? l) l)
                           ((> (car l) stop) l)
                           ((< (car l) start) 
                            (cons (car l) 
                                  (all-but-interval (cdr l) start stop)))
                           (else (all-but-interval (cdr l) start stop))))]}

@interaction[(all-but-interval '(1 2 3 4 5) 2 4)
             (all-but-interval '(1 2 3 4 5) 0 7)
             (all-but-interval '(1 2 3 4 5) 1 1)]


@section{Iteratieve versie}
Schrijf een iteratieve versie van dezelfde procedure.

@solution{@def+int[(define (all-but-interval l start stop)
                     (define (iter l acc)
                       (cond ((null? l) acc)
                             ((> (car l) stop) (append (reverse acc) l))
                             ((< (car l) start) (iter (cdr l) (cons (car l) acc)))
                             (else (iter (cdr l) acc))
                             ))
                     (iter l '()))]}

@interaction[(all-but-interval '(1 2 3 4 5) 2 4)
             (all-but-interval '(1 2 3 4 5) 0 7)
             (all-but-interval '(1 2 3 4 5) 1 1)]


@section{Zelfde oefening voor een lijst waar dubbels in voorkomen.}
Moet je de procedure veranderen als er wel dubbels in de lijst mogen voorkomen? Zo ja, herschrijf de eerste versie. Zo nee, waarom niet?

@solution{Neen, alles blijft hetzelfde.}

@interaction[(all-but-interval '(1 2 2 2 3 4 4 5 5) 2 4)]

