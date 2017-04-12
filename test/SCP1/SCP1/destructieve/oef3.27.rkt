#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(reset-r5rs-evaluator)
@hidden-code[(#%require (only racket/trace trace))]
@hidden-code[(define (make-table) (cons 'table '()))]
@hidden-code[(define (lookup x table)(let ((res (assoc x (cdr table))))
                                       (if res (cdr res) #f)))]
@hidden-code[(define (insert! x res table)
               (set-cdr! table (cons (cons x res) (cdr table))))]
@title{Oefening 3.27 uit Abelson&Sussman}
Gegeven:

@defs+int[((define (memoize f)
             (let ((table (make-table)))
               (define (lookup-or-compute x)
                 (let ((previously-computed-result
                        (lookup x table)))
                   (if previously-computed-result
                       previously-computed-result
                       (let ((result (f x)))
                         (insert! x result table)
                         result))))
               (trace lookup-or-compute)
               lookup-or-compute))
           
           (define memo-fib
             (memoize
              (lambda (n)
                (cond ((= n 0) 0)
                      ((= n 1) 1)
                      (else (+ (memo-fib (- n 1))
                               (memo-fib (- n 2)))))))))]


@section{Voorspel}
Voorspel de trace van @scheme[memo-fib] en @scheme[memoize] bij de oproep van @scheme[(memo-fib 3)]. Toon aan dat @scheme[memo-fib] het n-de Fibbonaci-getal berekent in O(n) indien je veronderstelt dat @scheme[look-up] O(1) is.

@solution{@interaction[(trace memo-fib)
                       (memo-fib 3)]}

@solution{Bewijs per inductie:
          Basis:
          T( fib(0) ) = 1 = O(1)
          T( fib(1) ) = 1 = O(1)
          
          Hypothese:
          T( fib(n) ) = O(n)
          
          Inductie:
          T( fib(n+1) ) = T( fib(n) ) + T( fib(n-1) )
          = O(n)        + 1   (hypothese + simpele look-up
          = O(n)}

@section{Memoize fib?}
Werkt dit nog indien we @scheme[memo-fib] simpelweg hadden gedefinieerd als @scheme[(memoize fib)]?

@solution{ Ja, maar niet zo performant als bedoeld.
          Het voordeel van 'memoization' is verloren binnen in het algoritme, omdat @scheme[memo-fib] intern de gewone 'fib'aanroept.
          Als @scheme[memo-fib] meerdere malen wordt opgeroepen, dan kan 'memoization' wel zijn werk doen.}

@section{Performantie calc-e}
Wat is de performantie van @scheme[calc-e] als we een gememoïzeerde versie van @scheme[factorial] gebruiken?

@schemeblock[((define (calc-e n)
                (if (= n 0)
                    1
                    (+ (/ 1 (factorial n))
                       (calc-e (- n 1))))))]

@solution{ O(n)
          In de eerste oproep, kost (fac n), O(n) tijd.
          Bij de (n-1) daarop volgende oproepen kost (fac k) slecht O(1)omdat k < n, en dus onthouden is.
          We krijgen dus: O(n) + n - 1 = O(n)}

@(reset-r5rs-evaluator)