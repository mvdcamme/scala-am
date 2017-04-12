#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Mutuele recursie}
Schrijf de procedures @scheme[(odd? n)] 
en @scheme[(even? n)] in functie van elkaar.

@solution[@defs+int[((define (odd? n)
                       (if (= n 0) #f (even? (- n 1))))
                     
                     (define (even? n)
                       (if (= n 0) #t (odd? (- n 1)))))]]