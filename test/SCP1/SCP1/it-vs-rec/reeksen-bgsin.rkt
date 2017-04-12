#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math

@title{Boog-sinus}
Schrijf een functie die de reeksontwikkeling voor @math-in{Bgsin(x)} voor
n termen berekent.
@math-disp{bgsin(x) = x + \frac{1}{2.3}x^3 + \frac{1.3}{2.4.5}x^5 + \frac{1.3.5}{2.4.6.7}x^7+...}
(Opmerking: deze reeksontwikkeling is enkel geldig voor @math-in{-\frac{\pi}{2} \leq bgsin(x) \leq \frac{\pi}{2}} )

@solution[@def+int[(define (bgsin x n)
                     (define (iter i ti som)
                       (if (= i n)
                           som
                           (let* ((2i+1 (+ (* 2 i) 1))
                                  (2i+2 (+ 2i+1 1))
                                  (2i+3 (+ 2i+1 2))
                                  (ti+1 (/ (* ti 2i+1 x x 2i+1)
                                           (* 2i+2 2i+3))))
                             (iter (+ i 1) ti+1 (+ som ti+1)))))
                     (iter 0 x x))]]