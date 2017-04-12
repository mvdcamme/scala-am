#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Combine-special}
@section{combine-special}
Definieer een functie @scheme[(combine-special f1 f2 comb)],
die van de twee eerste functies in één argument
een nieuwe functie maakt door ze te combineren met
de comb functie.
Voorbeeld: @scheme[(combine-special sin cos +)] levert de functie @scheme[(lambda (x) (+ (sinx) (cosx)))].
@solution[@def+int[(define (combine-special f1 f2 comb)
                     (lambda (x)
                       (comb (f1 x) (f2 x))))]]


Definieer d.m.v. combine-special de functie @math-in{\frac{(2x + 2(\sin{x} \times \cos{x}))}{x}}.
@solution[@defs+int[((define (id x) x)
                     (define (double x)(* x 2))
                     (define (compose f g)
                       (lambda (a b) (f (g a b))))
                     (define f (combine-special
                                (combine-special double
                                                 (combine-special sin cos  *)
                                                 id)
                                id /)))]]

