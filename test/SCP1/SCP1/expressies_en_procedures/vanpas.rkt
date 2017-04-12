#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Enkele procedures die later nog van pas kunnen komen}

Schrijf een procedure @scheme[incr] die bij zijn enige argument 1 optelt.
Schrijf ook een analoge procedure @scheme[decr] die van zijn enige argument
1 aftrekt.

@solution[@defs+int[((define (incr x) (+ x 1))
                     (define (decr x) (- x 1)))]]


Schrijf een expressie die aantoont dat als je @scheme[decr] oproept met het resultaat van 
een oproep van @scheme[incr], je weer het originele resultaat krijgt. 

@solution[@interaction[(decr (incr 999))(decr (incr 123))]]