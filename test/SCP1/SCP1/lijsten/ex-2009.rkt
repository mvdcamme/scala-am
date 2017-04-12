#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica augustus 2009}
@section[#:tag "vraag1"]{Vergelijken van twee lijsten}
Schrijf een functie @scheme[(compare lijst1 lijst2)] die twee niet-geneste lijsten lijst1 en lijst2 als invoer neemt, en als resultaat een getal (eventueel 0) teruggeeft dat uitdrukt tot waar beide lijsten dezelfde elementen bevatten (te beginnen vanaf het eerste element).

@solution{@def+int[(define (compare lijst1 lijst2)
                     (cond ((or (null? lijst1) (null? lijst2)) 0)
                           ((eq? (car lijst1) (car lijst2)) 
                            (+ 1 (compare (cdr lijst1) (cdr lijst2))))
                           (else 0)))]}

@interaction[(compare '(a b c d e f g) '(a b c x y))
             (compare '(x a b) '(a b c d e f g))
             (compare '(a b c e f g) '(a b))]

@section{Recursie/iteratie}
Levert je oplossing uit @secref{vraag1} een recursief of iteratief proces? Leg in je eigen woorden uit waarom je dit antwoord geeft (argumenteer bondig). Schrijf de andere versie voor @secref{vraag1}.

@solution{@def+int[(define (compare-iter lijst1 lijst2)
                     (define (loop l1 l2 res)
                       (cond ((or (null? l1) (null? l2)) res)
                             ((eq? (car l1) (car l2)) (loop (cdr l1) (cdr l2) (+ res 1)))
                             (else res)))
                     (loop lijst1 lijst2 0))]}

@interaction[(compare '(a b c d e f g) '(a b c x y))
             (compare '(x a b) '(a b c d e f g))
             (compare '(a b c e f g) '(a b))]


@section[#:tag "vraag3"]{Vergelijk twee lijsten met een bepaalde test}
Schrijf een variant van de functie @scheme[compare] die drie argumenten als invoer neemt, namelijk @scheme[lijst1], @scheme[lijst2] en een willekeurige vergelijkingstest. Deze test wordt gebruikt om de elementen van de twee lijsten te vergelijken. Het resultaat van deze variant is net zoals bij de functie uit @secref{vraag1} een getal dat uitdrukt tot waar beide lijsten voldoen aan de vergelijkingstest.

@solution{@def+int[(define (algemene-compare lijst1 lijst2 test)
                     (cond ((or (null? lijst1) (null? lijst2)) 0)
                           ((test (car lijst1) (car lijst2)) 
                            (+ 1 (algemene-compare (cdr lijst1) (cdr lijst2) test)))
                           (else 0)))]}

@section{Elementen groter?}
Maak gebruik van de functie die je in @secref{vraag3} geschreven hebt om na te gaan tot welke positie lijst1 groter is dan lijst2.

@solution{@def+int[(define (compare-greater lijst1 lijst2)
                     (algemene-compare lijst1 lijst2 >))]}

@interaction[(compare-greater '(3 5 6 1 2 5) '(2 1 0 8 5 5))]