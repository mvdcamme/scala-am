#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Max}
Definieer met behulp van @scheme[accumulate] een procedure @scheme[(max-interval f a b)] die een benadering 
geeft van de maximum waarde van een functie @scheme[f] op een interval @scheme[a]@scheme[b].

@def+int[(define (accumulate combiner null-value term a next b)
           (define (iter a res)
             (if (< a b)
                 (iter (next a) (combiner (term a) res))
                 res))
           (iter a null-value))]

Het is slechts een benadering omdat een interval eigenlijk oneindig veel
(zelfs overaftelbaar veel) elementen bevat.
Maak gebruik van de voorgedefinieerde Scheme-procedure @scheme[max].

@solution[@def+int[(define (max-interval f a b)
                     (define (incr i) (+ i 1))
                     (accumulate max (f a) f a incr b))]]

