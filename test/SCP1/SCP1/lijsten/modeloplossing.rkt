#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Recursie/iteratie op lijsten: modeloplossing}
Het volgende stukje "code" beschrijft een standaardpatroon voor recursieve procedures op lijsten.
De meeste recursieve procedures op lijsten kunnen geschreven worden volgens dit patroon, of volgens een zeer gelijkaardig patroon.

@schemeblock[(define (list-procedure-rec lst)
               (if (null? lst)
                   base-result
                   (combine-car/res (do-something-with (car lst))
                                    (list-procedure-rec (cdr lst)))))
             
             (define (list-procedure-it lst)
               (define (iter lst res)
                 (if (null? lst)
                     res
                     (iter (cdr lst)
                           (combine-car/res (do-something-with (car lst))
                                            res))))
               (iter lst base-result))]

