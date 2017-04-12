#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math

@title{cosinus}
Zelfde vraag voor de functie @math-in{cos(x)}, met de procedure @scheme[(calc-cos x n)],
die de volgende reeks-ontwikkeling heeft:
@math-disp{cos(x)=\frac{x^0}{0!} - \frac{x^2}{2!} + \frac{x^4}{4!} - \frac{x^6}{6!} + ...}

@solution[@def+int[(define (calc-cos x n) 
                     (define (iter ctr acc fac xpow sign)
                       (if (>= ctr n)
                           acc
                           (let* ((i (* 2 ctr))
                                  (newfac (* fac (- i 1) i))
                                  (newxpow (* xpow x x))
                                  (newsign (- sign)))
                             (iter (+ ctr 1) 
                                   (+ acc (/ (* newsign newxpow) newfac)) 
                                   newfac 
                                   newxpow 
                                   newsign))))
                     (iter 1 1 1 1 1))]]

@interaction[(calc-cos 0 10)
             (calc-cos (/ 3.1415 2) 10)
             (calc-cos 3.1415 10)]