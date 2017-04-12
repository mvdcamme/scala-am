#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica Partieel januari 1997}
Veronderstel dat we een woord voorstellen als een opeenvolging van symbolen in een lijst. Bijvoorbeeld "hottentottententen" wordt voorgesteld als @scheme['(h o t t e n t o t t e n t e n t e n)].

@section[#:tag "vraag1"]{Tel opeenvolging van letters}
Schrijf op recursieve of iteratieve wijze een procedure @scheme[count-2-consecutive] die telt hoeveel keer 2 gegeven letters onmiddellijk na elkaar in een gegeven woord voorkomen.

@solution{@def+int[(define (count-2-consecutive first second lst)
                     (define (count previous lst)
                       (if (null? lst) 0
                           (let ((current (car lst)))
                             (+ (if (and (equal? previous first)
                                         (equal? current second))
                                    1
                                    0)
                                (count current (cdr lst))))))
                     (if (null? lst) 0
                         (count (car lst) (cdr lst))))]}

@interaction[(count-2-consecutive 'n 't '(h o t t e n t o t t e n t e n t e n))
             (count-2-consecutive 'e 'n '(h o t t e n t o t t e n t e n t e n))]


@section[#:tag "vraag2"]{Recursie/iteratie}
Genereert de procedure die je geschreven hebt in @secref{vraag1} een recursief of iteratief proces? Schrijf indien mogelijk een procedure die het andere proces genereert. Indien niet mogelijk, leg uit waarom.

@solution{@def+int[(define (count-2-consecutive first second lst)
                     (define (count previous lst ctr)
                       (if (null? lst) ctr
                           (let ((current (car lst))
                                 (rest (cdr lst)))
                             (if (and (equal? previous first)
                                      (equal? current second))
                                 (count current rest (+ ctr 1))
                                 (count current rest ctr)))))
                     (if (null? lst) 0
                         (count (car lst) (cdr lst) 0)))]}

@interaction[(count-2-consecutive 'n 't '(h o t t e n t o t t e n t e n t e n))
             (count-2-consecutive 'e 'n '(h o t t e n t o t t e n t e n t e n))]


@section{Veralgemening}
Veralgemeen naar keuze je procedure uit @secref{vraag1} of @secref{vraag2} tot een procedure @scheme[count-n-consecutive] die als invoer een lijst van letters en een woord krijgt, en telt hoeveel keer de gegeven sequentie van letters in het woord voorkomt.

Let op! Hou rekening met het feit dat in een woord zoals "barbapapa" de combinatie van letters "apa" twee keer voorkomt, maar dat de combinaties elkaar overlappen. De letter "a" die in de ene combinatie (barb@bold{apa}pa) als laatste letter wordt gebruikt, wordt in de tweede combinatie (barbap@bold{apa}) als eerste letter gebruikt.

@solution{@def+int[(define (count-n-consecutive letter-list lst)
                     (define (count lst ctr)
                       (if (< (length lst) (length letter-list))
                           ctr
                           (if (compare? letter-list lst)
                               (count (cdr lst) (+ ctr 1))
                               (count (cdr lst) ctr))))
                     (count lst 0))
                   
                   (define (compare? letters lst)
                     (if (null? letters)
                         #t
                         (and (equal? (car letters) (car lst))
                              (compare? (cdr letters) (cdr lst)))))]}

@interaction[(count-n-consecutive '(t e n)
                                  '(h o t t e n t o t t e n t e n t e n))
             (count-n-consecutive '(o t t e n)
                                  '(h o t t e n t o t t e n t e n t e n))]

