#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Append}
Schrijf de procedure @scheme[append] op iteratieve wijze. Doe dit zo efficiënt mogelijk.

@solution[@def+int[(define (append lst1 lst2)
                     (define (loop lst res)
                       (if (null? lst)
                           res
                           (loop (cdr lst) (cons (car lst) res))))
                     (loop (reverse lst1) lst2))]]

@interaction[(append '(1 2 3) '(4 5))
             (append '(1 2 3) '())
             (append '() '(1 2 3))
             (append '() '())]

@(reset-r5rs-evaluator)