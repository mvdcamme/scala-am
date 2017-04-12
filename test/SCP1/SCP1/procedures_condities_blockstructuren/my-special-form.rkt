#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{``Lazy evaluation'' van special forms}

@section{my-and}
Schrijf, zonder gebruik te maken van de
voorgedefinieerde Scheme special form @scheme[and],
je eigen functie @scheme[my-and], die twee parameters neemt en
enkel @scheme[#t] teruggeeft indien ze beide @scheme[#t] zijn.
Doe dit met behulp van een @scheme[if].

@solution[@def+int[(define (my-and a b)(if a b #f))]]


@section{predict}
Probeer nu de volgende 4 expressies uit en verklaar dit resultaat:
@itemize[@item[@predict[(my-and (> 1 2) (1))]]
          @item[@predict[(and (> 1 2) (1))]]
          @item[@predict[(my-and (< 1 2) (1))]]
          @item[@predict[(and (< 1 2) (1))]]]



@section{my-or}
Schrijf een analoge functie @scheme[my-or]
(zonder gebruik te maken van de special form @scheme[or]) en
geef zelf een expressie die zich voor deze @scheme[my-or] verschillend gedraagt
dan wanneer je de standaard Scheme @scheme[or] gebruikt.
@solution[@def+int[(define (my-or a b)(if a #t b))]]


@section{my-if}
Definieer tenslotte zelf een functie @scheme[my-if].
Hierbij mag je geen gebruik maken van de bestaande special form @scheme[if],
maar wel van de @scheme[cond] special form.
@solution[@def+int[(define (my-if condition consequence alternative)
                     (cond
                       ((condition consequence))
                       (else alternative)))]]


@section{check devision by zero}
Voorspel, verklaar en verifieer het
resultaat van @scheme[(check-db0-if 6 0)] en @scheme[(check-db0-my-if 6 0)] 
indien de procedures @scheme[(check-db0-if 6 0)]
en @scheme[(check-db0-my-if 6 0)] als volgt gedefinieerd zijn:

@defs+int[((define (check-db0-if x y)
             (if (= y 0)
                 x
                 (/ x y)))
           
           (define (check-db0-my-if x y)
             (my-if (= y 0)
                    x
                    (/ x y))))]

@itemize[@item[@predict[(check-db0-if 6 0)]]
          @item[@predict[(check-db0-my-if 6 0)]]]