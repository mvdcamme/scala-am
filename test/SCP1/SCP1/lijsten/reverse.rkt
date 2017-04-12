#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 2.17 uit Abelson&Sussman}
Schrijf de procedure @scheme[(reverse l)]
die een lijst teruggeeft met dezelfde
elementen als l maar in de omgekeerde
volgorde.

Maak 2 versies: één die een recursief
proces en één die een iteratief proces
genereert.
Denk eens na over de performantie.

@solution{@def+int[(define (reverse l)
                     (if (null? l)
                         '()
                         (append (reverse (cdr l))
                                 (list (car l)))))]}

@solution{@def+int[(define (reverse l)
                     (define (iter l r)
                       (if (null? l)
                           r
                           (iter (cdr l)
                                 (cons (car l) r))))
                     (iter l '()))]}

@interaction[(reverse '(1 2 3))
             (reverse '(1))
             (reverse '())]
