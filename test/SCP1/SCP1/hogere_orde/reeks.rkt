#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")



@title{Gebruik van accumulate}

In de theorie hebben we een procedure @scheme{accumulate}
als volgt gedefinieerd:
@def+int[(define (accumulate combiner null-value term a next b)
           (define (iter a res)
             (if (< a b)
                 (iter (next a) (combiner (term a) res))
                 res))
           (iter a null-value))]

@section{Reeksontwikkeling}
Definieer @scheme[calc-e],@scheme[calc-sin] en @scheme[calc-cos] (zie Oefeningensessie 3)
met behulp van @scheme[accumulate]. Weeg aan de hand van deze oefening de voor- en
nadelen af van het gebruik van hogere orde functies zoals @scheme[accumulate].

@solution[@defs+int[((define (factorial n) 
                       (if (zero? n)
                           1
                           (* n (factorial (- n 1)))))
                     
                     (define (calc-e)
                       (define (term i)(/ 1 (factorial i)))
                       (define (incr i)(+ i 1))
                       (accumulate + 1 term 0 incr 100))
                     
                     (define (calc-sin x)
                       (define (incr x) (+ x 1))
                       (define (term i)
                         ((if (= (modulo i 2)0) - +)
                          (let ((i (+ (* 2 i) 1)))
                            (/ (expt x i)(factorial i)))))
                       (accumulate + 0 term 0 incr 100))
                     
                     (define (calc-cos x)
                       (define (incr x) (+ x 1))
                       (define (term i)
                         ((if (= (modulo i 2)0) + -)
                          (let ((i (* 2 i)))
                            (/ (expt x i)(factorial i)))))
                       (accumulate + 0 term 0 incr 100)))]]

