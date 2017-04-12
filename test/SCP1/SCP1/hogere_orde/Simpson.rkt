#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Oefening 1.29 uit Abelson and Sussman}

Schrijf de procedure @scheme[(simp-int f a b n)] die de
integraal van een functie f tussen a en b benadert d.m.v. de regel van Simpson:
@math-in{\frac{h}{3}\left( y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + \dots + 2y_{n-2} + 4y_{n-1} + y_n \right)}
met @math-in{h = \frac{b-a}{n}}, en @math-in{y_k = f(a + kh)}.

Hint: Maak gebruik van de hogere-orde procedure @scheme[sum]:

@def+int[(define (sum term a next b)
           (if (> a b)
               0
               (+ (term a) (sum term (next a) next b))))]



@solution[@def+int[(define (incr x) (+ x 1))
                   
                   (define (simp-int f a b n)
                     (let ((h (/ (- b a) n)))
                       (define (y k) (f (+ a (* h k))))
                       (define (term k)
                         (* (if (or (= k 0)(= k n)) 1 (+ 2 (* 2 (modulo k 2))))
                            (y k)))
                       (/ (* h (sum term 0 incr n)) 3)))]]

@interaction[(simp-int (lambda (x) x) 0 10 100)
             (define r (sqrt 2))
             (simp-int (lambda (x) (sqrt (- (* r r) (* x x)))) (- r) r 100)]

