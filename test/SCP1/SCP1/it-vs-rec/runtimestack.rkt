#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Gebruik van de runtime-stack}
Geef de output van de uitdrukking 
@scheme[(count1 4)] en @scheme[(count2 4)] indien 
@scheme[count1] en @scheme[count2] als volgt gedefinieerd zijn:

@defs+int[((define (count1 x)
             (cond ((= 0 x) (display x))
                   (else (display x)
                         (count1 (- x 1)))))
           
           (define (count2 x)
             (cond ((= 0 x) (display x))
                   (else (count2 (- x 1))
                         (display x)))))]


@solution[@interaction[(count1 4)(count2 4)]]