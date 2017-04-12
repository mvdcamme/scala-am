#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Som Herbekeken}
Veronderstel dat je beschikt over een voorgedefinieerde
increment-functie @scheme[1+] (resp. decrement-functie @scheme[1-])
die één enkel argument neemt en er 1 bij optelt (resp. van aftrekt).
Definieer nu zelf de optelling @scheme[(add a b)] aan de hand van deze
increment- en decrement-functies.
@defs+int[((define (1- x) (- x 1))
           (define (1+ x) (+ 1 x)))]

@section{Recursief}
Schrijf eerst een versie die een recursief process genereert.

@solution[@def+int[(define (rec-add a b)
                     (if (= b 0)
                         a
                         (1+ (rec-add a (1- b)))))]]

@interaction[(rec-add 4 5)]

@section{Iteratief}
Schrijf nu een versie die een iteratief process genereert.
@solution[@def+int[(define (iter-add a b)
                     (cond
                       ((= a 0) b)
                       ((< a 0) (iter-add (1+ a) (1- b)))
                       ((> a 0) (iter-add (1- a) (1+ b)))))]]

@interaction[(iter-add 4 5)]