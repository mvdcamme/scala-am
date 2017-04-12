#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 1.31 uit Abelson and Sussman}
Beschouw de hogere-orde procedure sum uit Abelson and Sussman blz.58.
@def+int[(define (sum term a next b)
           (if (> a b)
               0
               (+ (term a) (sum term (next a) next b))))]

@section{Product}
Definieer de procedure @scheme[(product factor a next b)], naar analogie met de hogere-orde procedure sum uit vorige oefening.
@solution[@def+int[(define (product factor a next b)
                     (if (> a b)
                         1
                         (* (factor a) (product factor (next a) next b))))]]

@section{Product - iteratieve wijze}
Schrijf de procedure @scheme[product] nu ook op iteratieve wijze.
@solution[@def+int[(define (iter-product factor a next b)
                     (define (iter a res)
                       (if (> a b)
                           res
                           (iter (next a) (* (factor a) res))))
                     (iter 1 a))]]

@section{Factorial}
Schrijf @scheme[factorial] met behulp van @scheme[product].
@solution[@def+int[(define (factorial n)
                     (define (incr x) (+ x 1))
                     (define (id x) x)
                     (product id 1 incr n))]]

