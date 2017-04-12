#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen januari 2004: Destructief}
Schrijf destrucief een functie @scheme[(insert! lst1 lst2)] die twee lijsten @scheme[lst1] en @scheme[lst2] als parameter neemt.
De lijst @scheme[lst1] is een lijst van (niet lege!) lijstjes, de lijst @scheme[lst2] is een lijst met symbolen.
De functie @scheme[insert!] voegt de symbolen van @scheme[lst2] toe aan de achterkant van de lijstjes in @scheme[lst1].

Hou er rekening mee dat je geen enkele nieuwe cons-cel mag aanmaken! Je mag alleen maar bestaande cons-cellen wijzigen.
Je mag ervan uitgaan dat de lijstjes @scheme[lst1] en @scheme[lst2] even lang zijn.

@solution{@defs+int[((define (insert-aux! lst lst2)
                       (set-cdr! lst2 '())
                       (if (null? (cdr lst))
                           (set-cdr! lst lst2)
                           (insert-aux! (cdr lst) lst2))
                       lst)
                     
                     (define (insert! lst1 lst2)
                       (if (not (null? lst1))
                           (begin
                             (insert! (cdr lst1) (cdr lst2))
                             (insert-aux! (car lst1) lst2)
                             lst1))))]
           
           @interaction[(insert-aux! '(a 12 q) '(v w x y z))]}


@interaction[(insert! '((a 12 q) (b 13) (c 14 r s) (f 18) (j 22 t)) '(v w x y z))]