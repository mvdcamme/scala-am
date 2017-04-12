#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 1.33 uit Abelson and Sussman}

@section{Filtered-accumulate}
Veralgemeen de procedure  @scheme[accumulate] naar de procedure @scheme[filtered-accumulate].
Deze procedure heeft een extra argument @scheme[filter?]: dit een predikaat dat
bepaalt welke termen er geaccumuleerd worden en welk niet. Zorg ervoor dat je
procedure een iteratief process genereert.

@solution[@def+int[(define (filtered-accumulate combiner filter? null-value term a next b)
                     (define (iter a res)
                       (if (< a b)
                           (iter (next a) 
                                 (if (filter? a)
                                     (combiner (term a) res)
                                     res))
                           res))
                     (iter a null-value))]]


@section{Grootste gemene deler}
Schrijf een procedure @scheme[(product-gcd n)] die het product
berekent van alle integers @math-in{i < n} waarvoor geldt dat
@scheme[(= (gcd i n) 1)].
(Opmerking: @scheme[gcd] is een voorgedefinieerde
Scheme- procedure die de grootste gemene deler
van 2 getallen berekent.)

@solution[@def+int[(define (product-gcd n)
                     (define (incr x) (+ x 1))
                     (define (id x) x)
                     (define (filter? a)
                       (= (gcd a n) 1))
                     (filtered-accumulate * filter? 1 id 1 incr n))]]

