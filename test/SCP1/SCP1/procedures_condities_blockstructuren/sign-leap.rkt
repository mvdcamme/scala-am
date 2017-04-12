#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Gebruik van conditionele expressies}
@section{Sign}
Definieer een procedure @scheme[(sign number)] die 
een getal als argument neemt en
@scheme[1] teruggeeft als dat nummer positief is,
@scheme[-1] als het negatief is en
@scheme[0] als het getal gelijk is aan 0.

@solution[@def+int[(define (sign number)
                     (cond ((zero? number) 0)
                           ((> number 0) 1)
                           (else -1)))]]


@interaction[(sign -5)(sign 17.28)(sign 0)]


@section{leap-year?}
Definieer een predicaat @scheme[leap-year?] dat
een jaartal als argument neemt en
teruggeeft of het betreffende jaar een schrikkeljaar is.
Alle jaren die deelbaar zijn door 400 zijn schrikkeljaren,
alle jaren die deelbaar zijn door 100 (maar niet door 400) 
zijn geen schrikkeljaren, en alle jaren die deelbaar zijn door
4 (maar niet door 100) zijn wel schrikkeljaren.
Alle jaren die niet aan de voorgaande voorwaarden
voldoen zijn dan weer geen schrikkeljaren.

@solution[@defs+int[((define (divides? deler deeltal)
                       (= 0  (modulo deeltal deler)))
                     
                     (define (leap-year? year)
                       (if (divides? 4 year)
                           (if (divides? 100 year)
                               (divides? 400 year)
                               #t)
                           #f))
                     
                     (define (leap-year2? year)
                       (cond ((divides? 400 year) #t)
                             ((divides? 100 year) #f)
                             ((divides? 4 year) #t)
                             (else #f)))
                    
                    (define (leap-year3? year)
                      (if (divides? 400 year)
                          #t
                          (if (divides? 100 year)
                              #f
                              (divides? 4 year))))
                    
                    
                    (define (leap-year4? year)
                      (or (divides? 400 year)
                          (and (divides? 4 year)
                               (not (divides? 100 year))))))]]


@interaction[(leap-year? 1989)
             (leap-year? 2000)
             (leap-year? 1900)]

Hint: Je definieert best een predicaat @scheme[divides?]
dat teruggeeft of een getal exact een ander getal deelt.
