#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica 2de zit 1995}
Een procedure @scheme[schuif-in!] neemt als invoer twee lijsten en transformeert deze twee lijsten in 1 enkele lijst door afwisselend een element van de eerste en van de tweede lijst te nemen.

Schrijf nu een destructieve versie @scheme[schuif-in!] van de procedure.
D.w.z. dat geen enkele nieuwe cons-cel mag aangemaakt worden.
De procedure @scheme[schuif-in!] neemt als invoer de twee gegeven lijsten, en transformeert de eerste ervan op destructieve wijze in het gewenste resultaat.

@solution{@defs+int[((define (schuif-in! l1 l2)
                       (cond ((null? (cdr l1)) (set-cdr! l1 l2) 'ok)
                             ((null? l2) 'ok)
                             (else
                              (let ((rest1 (cdr l1)) (rest2 (cdr l2)))
                                (set-cdr! l1 l2)
                                (set-cdr! l2 rest1)
                                (schuif-in! rest1 rest2))))))]}


@interaction[(define lijst1 '(1 3 5))]
@interaction[(define lijst2 '(2 4 6 8))]
@interaction[(schuif-in! lijst1 lijst2)]
@interaction[lijst1]

