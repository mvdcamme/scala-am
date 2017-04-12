#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica 2eZit 1994}

@section{display-n}
Schrijf een recursieve procedure @scheme[(display-n x n)] die
twee parameters aanvaardt, een karakter en
een positief geheel getal. Ze zal — gebruik
makende van de standaard procedure display — het
gegeven karakter zoveel maal afdrukken als de
tweede parameter aangeeft.
@solution[@def+int[(define (display-n x n)
                     (if (> n 0)
                         (begin
                           (display x)
                           (display-n x (- n 1)))))]]

@interaction[(display-n '* 4)]

@section{squares}

Schrijf een recursieve procedure @scheme[squares]
die een positief geheel getal aanvaardt,
en vier vierkanten (met als zijdelengte het gegeven getal)
afdrukt, zodanig dat het geheel opnieuw een vierkant vormt.
(Noot: je mag ervan uitgaan dat alle karakters even groot op
het scherm worden afgedrukt.)


@solution[@def+int[(define (squares n)
                     (define (display-full-line)
                       (display-n "*" (* n 2))
                       (newline))
                     (define (display-semi-semi-line)
                       (display "*")
                       (display-n " " (- n 2))
                       (display "*"))
                     (define (displaysemi-line)
                       (display-semi-semi-line)
                       (display-semi-semi-line)
                       (newline))
                     (define (semi-line-iter i)
                       (if (< i (- n 2))
                           (begin
                             (displaysemi-line)
                             (semi-line-iter (+ i 1)))))
                     (display-full-line)
                     (semi-line-iter 0)
                     (display-full-line)
                     (semi-line-iter 0)
                     (display-full-line))]]

@interaction[(squares 3)(squares 5)]

@section{explain}
Zeg voor elk van je oplossingen in (a) en (b) of ze 
een iteratief dan wel een recursief proces genereren. 
Argumenteer waarom.
