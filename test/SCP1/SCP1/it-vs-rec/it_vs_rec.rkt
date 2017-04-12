#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math

@title{Multiply}
In paragraaf 1.2.4 van Abelson&Sussman wordt de
machtsverheffing gedefinieerd in termen van de vermenigvuldiging.
Definieer nu op analoge wijze de vermenigvuldiging aan de hand van de
optelling. Schrijf hiervoor een functie @scheme[(multiply a b)],
die twee positieve getallen vermenigvuldigt, en die als volgt werkt:

@math-disp{a \times 0 = 0\\a \times 1 = (a \times 0) + a\\a \times 2 = (a \times 1) + a}

Genereert je definitie een iteratief of een recursief proces?
Zet het een om in het ander.

@solution[@def+int[(define (rec-multiply a b)
                     (if (zero? b)
                         0
                         (+ a (rec-multiply a (- b 1)))))]]

@interaction[(rec-multiply 5 2)]

@solution[@def+int[(define (iter-multiply a b)
                     (define (iter result counter)
                       (if (zero? counter)
                           result
                           (iter (+ result a) (- counter 1))))
                     (iter 0 b))]]

@interaction[(iter-multiply 5 2)]

@section{Fast Multiply}
Veronderstel dat je beschikt over procedures @scheme[(double a)] en
@scheme[(halve a)] die hun argument verdubbelen respectievelijk halveren.
Definieer een functie @scheme[fast-multiply] die hetzelfde is als de
@scheme[multiply] uit vraag a),
maar die in logaritmische tijd haar resultaat berekent.
Doe dit zowel op recursieve als op iteratieve (= staartrecursieve) wijze.

Hint: Gebruik de analogie met de definitie van  @scheme[fast-expt] uit Abelson&Sussman blz. 45.

@defs+int[((define (double x) (+ x x))
           
           (define (halve x) (/ x 2)))]

Recursief proces (m.a.w. niet staart-recursief):
@solution[@def+int[(define (rec-fast-multiply a b)
                     (cond ((zero? b) 0)
                           ((even? b) (rec-fast-multiply (double a) (halve b)))
                           (else (+ a (rec-fast-multiply a (- b 1))))))]]

@interaction[(rec-fast-multiply 3 4)(rec-fast-multiply 100 200)]

@solution[@def+int[(define (iter-fast-multiply a b)
                     (define (iter a b acc)
                       (cond ((zero? b) acc)
                             ((even? b) (iter (double a) (halve b) acc))
                             (else (iter a (- b 1) (+ acc a)))))
                     (iter a b 0))]]

@interaction[iter-fast-multiply
             (iter-fast-multiply 3 4)
             (iter-fast-multiply 100 200)]