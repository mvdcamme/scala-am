#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Recursie/iteratie op geneste lijsten: modeloplossing}
Het volgende stukje "pseudo-code" beschrijft een standaardpatroon van een boomrecursieve procedure.
Bijna elke boomrecursieve procedure zal geschreven zijn volgens dit patroon, of een zeer sterk gelijkend patroon.

@schemeblock[(define (atom? x)
               (not (pair? x)))
             
             (define (tree-procedure-rec lst)
               (cond ((null? lst) base-result)
                     ((atom? lst) atom-result)
                     (else (combine-branches (tree-procedure-rec (car lst))
                                             (tree-procedure-rec (cdr lst))))))]



