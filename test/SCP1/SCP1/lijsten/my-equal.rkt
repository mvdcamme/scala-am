#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 2.54 uit Abelson&Sussman}
Schrijf de procedure @scheme[(my-equal l1 l2)] die nagaat of twee lijsten van symbolen @scheme[l1] en @scheme[l2] gelijk zijn. Gebruik @scheme[(eq? e1 e2)] om twee elementen te vergelijken.

@solution{@def+int[(define (my-equal? l1 l2)
                     (cond ((and (null? l1) (null? l2)) #t)
                           ((or (null? l1) (null? l2)) #f)
                           ((equal? (car l1) (car l2)) (my-equal? (cdr l1) (cdr l2)))
                           (else #f)))]}

@interaction[(my-equal? '(1 2 3) '(1 2 3))
             (my-equal? '(1 2 3) '(4 5 6))
             (my-equal? '(1 2 3) '(1 2))
             (my-equal? '(1 2 3) '())
             (my-equal? '() '(1 2 3))]
