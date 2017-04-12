#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Iteratie met een teller}

@section{power-close-to}

Schrijf een procedure @scheme[(power-close-to b n)] die twee positieve integers 
@scheme[b] en @scheme[n] als argumenten heeft en de kleinste integer @scheme[e] teruggeeft waarvoor geldt dat 
@math-in{b^e > n}.
Doe dit door gebruik te maken van de Scheme-procedure @scheme[expt].

Voorbeeld:
@interaction[(expt 2 3)]
want
@interaction[(* 2 2 2)]

@solution[@def+int[(define (power-close-to b n)
                     (define (iter e)
                       (if (> (expt b e) n)
                           e
                           (iter (+ e 1))))
                     (iter 0))]]


@section{performantie}
Wat is de performantie van deze procedure?

@solution{Het process dat beschreven wordt door de procedure
          @scheme[iter] wordt  @math-in{\frac{\log{n}}{\log{b}}} keer uitgevoerd.
          Als we er van uitgaan dat @scheme[(expt n e)] in @scheme[e] stappend wordt uitgevoerd, dan is de
          performantie van onze @scheme[expt-power-close-to]  @math-in{O(\frac{\log{n}}{\log{b}}^2)}}

@section{Efficiënter}
Schrijf de procedure efficiënter door haar iteratief te schrijven en 
aan elke nieuwe stap de macht uit de vorige stap door te geven.
@solution{@def+int[(define (power-close-to++ b n)
                     (define (iter e macht)
                       (if (> macht n)
                           e
                           (iter (+ e 1) (* macht b))))
                     (iter 0 1))]
           De performantie van @scheme[eff-power-close-to] is  @math-in{O(\frac{\log{n}}{\log{b}})},
           omdat nu de oproep naar @scheme[expt] niet meer nodig is.}

@interaction[(power-close-to 2 1000)(power-close-to++ 2 1000)]