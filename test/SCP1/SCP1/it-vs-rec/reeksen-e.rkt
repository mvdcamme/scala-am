#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math

@title{Het getal @math-in{e}}
Onderstaande reeksontwikkeling heeft als som het getal @math-in{e = 2,718281828...}.
@math-disp{e = \frac{1}{0!} +\frac{1}{1!} +\frac{1}{2!} +\frac{1}{3!} + ...}
@scheme[(calc-e n)] geeft de som van de eerste @math-in{n+1} termen van de reeks:

@defs+int[((define (factorial n) 
             (if (zero? n)
                 1
                 (* n (factorial (- n 1)))))
           
           (define (calc-e n)
             (if (= n 0)
                 1
                 (+ (/ 1 (factorial n))
                    (calc-e (- n 1))))))]


Hierbij geeft @scheme[(factorial n)] het resultaat @math-in{n!},
waarbij precies @scheme[n] vermenigvuldigingen worden gedaan
(zie Abelson&Sussman blz. 32).
Hoeveel vermenigvuldigingen neemt @scheme[(calc-e n)] dan in beslag?
Verander de definitie van @scheme[calc-e] zodat je in totaal precies @scheme[n]
vermenigvuldigingen doet.

Hint: Verweef de definities van @scheme[calc-e] en @scheme[factorial] met elkaar.


@solution[@def+int[(define (calc-e-iter n)
                     (define (iter ctr res fac-prev)
                       (if (> ctr n)
                           res
                           (let ((new-fac (* ctr fac-prev)))
                             (iter (+ ctr 1) (+ res (/ 1 new-fac)) new-fac))))
                     (iter 1 1 1))]]

@interaction[(exact->inexact (calc-e 10))]