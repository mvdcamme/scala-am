#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 1.32 uit Abelson and Sussman}

@section{Accumulate}
Schrijf de procedure @scheme[(accumulate combiner null-value term a next b)],
die een veralgemening is van de procedures sum en product.
Schrijf accumulate op iteratieve wijze.
combiner is een procedure die specificeert hoe de huidige term moet `
gecombineerd worden met de accumulatie van de voorgaande termen.
null-value specificeert de initiëele waarde van de accumulatie.

@solution[@def+int[(define (accumulate combiner null-value term a next b)
                     (define (iter a res)
                       (if (< a b)
                           (iter (next a) (combiner (term a) res))
                           res))
                     (iter a null-value))]]


@section{Product and sum}
Schrijf @scheme[product] en @scheme[sum] met behulp van @scheme[accumulate].

@solution[@defs+int[((define (const v)
                       (lambda (i) v))
                     (define (incr x) (+ x 1))
                     
                     (define (product a b)
                       (accumulate + 0 (const a) 0 incr b))
                     
                     (define (sum a b)
                       (accumulate + a (const 1) 0 incr b)))]]

