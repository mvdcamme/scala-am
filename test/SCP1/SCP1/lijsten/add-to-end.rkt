#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Element toevoegen achteraan een lijst}
Defineer (zonder gebruik te maken van @scheme[append]) een procedure @scheme[(add-to-end e l)] die een nieuwe lijst teruggeeft met dezelfde elementen als de lijst @scheme[l] maar met het object @scheme[e] aan het einde toegevoegd.

@solution{@def+int[(define (add-to-end e l)
                     (if (null? l)
                         (cons e '())
                         (cons (car l) (add-to-end e (cdr l)))))]}

@interaction[(add-to-end 999 '(1 2 3 4 5))
             (add-to-end 999 '())
             (add-to-end 999 '(1))]