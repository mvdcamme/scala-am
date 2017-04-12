#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math

@title{sinus}
Onderstaande reeksontwikkeling berekent @math-in{sin(x)}.
@math-disp{sin(x)=\frac{x}{1!} - \frac{x^3}{3!} + \frac{x^5}{5!} - \frac{x^7}{7!} + ...}
Definieer een procedure @scheme[(calc-sin x n)], die de som van de eerste n 
termen berekent op een zo efficiënt mogelijke manier.
Geef de formule die je het aantal vermenigvuldigingen oplevert voor n termen.


@solution[@def+int[(define (calc-sin x n)
                     (define (iter ctr acc fac xpow sign)
                       (if (> ctr n)
                           acc
                           (let* ((i (- (* 2 ctr) 1))
                                  (newfac (* fac (- i 1) i))
                                  (newxpow (* xpow x x))
                                  (newsign (- sign)))
                             (iter (+ ctr 1) 
                                   (+ acc (/ (* newsign newxpow) newfac)) 
                                   newfac 
                                   newxpow 
                                   newsign))))
                     (iter 2 x 1 x 1))]]

@interaction[(calc-sin 0 10)
             (calc-sin (/ 3.1415 4) 10)
             (calc-sin (/ 3.1415 2) 10)]