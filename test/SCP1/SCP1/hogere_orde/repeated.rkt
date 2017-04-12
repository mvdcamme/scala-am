#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{repeated}
Definieer een procedure @scheme[(repeated f n)]
die een procedure teruggeeft die @scheme[f] @math-in{n} maal toepast,
m.a.w. die @scheme[(f (f ... (f x)))] berekent.
Doe dit zowel op recursieve als op iteratieve wijze.

@solution[@defs+int[((define (repeated f n)
                       (lambda (x)
                         (if (= n 0)
                             n
                             (f ((repeated f (- n 1)) x)))))
                     
                     (define (iter-repeated f n)
                       (lambda (x)
                         (define (iter i res)
                           (if (< i n)
                               (iter (+ i 1) (f res))
                               res))
                         (iter 0 n))))]]

