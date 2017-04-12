#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Druk hallo}
@section{Do-n}
Schrijf een procedure @scheme[(do-n f n)] die een parameterloze procedure en een getal n als 
invoer krijgt, en deze procedure n keer uitvoert.
@def+int[(define (druk-hallo)
           (display "hallo")
           (newline))]

@solution[@def+int[(define (do-n f n)
                     (if (> n 0)
                         (begin
                           (f)
                           (do-n f (- n 1)))))]]

@predict[(do-n druk-hallo 2)]


@section{Make-do-n}
Schrijf vervolgens (zonder gebruik te maken van @scheme[do-n])
een analoge procedure @scheme[make-do-n] die een parameterloze
procedure (bvb. @scheme[druk-hallo]) als invoer krijgt, en als
resultaat een parameterloze procedure teruggeeft die
bij aanroep hetzelfde doet als de ingevoerde parameterloze
procedure, maar dan n keer. Hoe zou je @scheme[make-do-n] dan gebruiken
om 4 keer hallo af te drukken?
@solution[@def+int[(define (make-do-n f n)
                     (lambda ()
                       (define (iter i)
                         (if (< i n)
                             (begin
                               (f)
                               (iter (+ i 1)))))
                       (iter 0)))]]
@itemize{@item[@predict[(make-do-n druk-hallo 2)]]
          @item[@predict[((make-do-n druk-hallo 2))]]}

@section{Verband}
Wat is het verband tussen @scheme[do-n] en @scheme[make-do-n]?
(M.a.w. schrijf @scheme[make-do-n] door gebruik te maken van @scheme[do-n].)
@solution[@def+int[(define (make-do-n2 f n)
                     (lambda () (do-n f n)))]]

