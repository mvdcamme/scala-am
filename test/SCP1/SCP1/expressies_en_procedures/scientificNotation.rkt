#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math @; put this ine ach file that uses formula's!

@title{Definitie van procedures (oefening 4.8 uit Simply Scheme)}

@section{Wetenschappelijke Notatie}
De wetenschappelijke notatie van een getal
is een manier om zeer grote of zeer kleine getallen
weer te geven als een aantal malen een gehele macht van 10.
Bijvoorbeeld, @math-in{5 \times 10^7} representeert het getal 50000000,
en @math-in{3.26 \times 10^{-9}} drukt het getal 0.00000000326 uit in
wetenschappelijke notatie.
Schrijf een procedure @scheme[(scientific getal exponent)] met twee argumenten:
een getal en een exponent van 10 die de corresponderende waarde teruggeeft:

Je kan hierbij gebruik maken van de procedure @scheme[(expt n e)]
die @math-in{n^e} uitrekent.
Merk op dat sommige versies van Scheme breuken als @scheme[a/b] weergeen. 
Andere versies gebruiken de wetenschappelijke notatie.
Dus het laatste voorbeeld kan gerepresenteerd worden als @scheme{21/5000} of
@scheme{4.2E-4} in plaats van @scheme[0.00042].

@solution[@def+int[(define (scientific getal exponent)
                     (exact->inexact (* getal (expt 10 exponent))))]]


@interaction[(scientific 7 3)
             (scientific 42 -5)]

@section{Een stapje moeilijker}
Kan je de procedures schrijven die de andere richting berekenen:
Je kan gebruik maken van de primitieve procedures @scheme[log] en @scheme[floor].

@solution{@defs+int[((define (sci-exponent getal)
                       (floor (/ (log getal) (log 10))))
                     
                     (define (sci-coefficient getal)
                       (/ getal (expt 10 (sci-exponent getal)))))]
           
           Merk op dat @scheme[(sci-exponent 1000)]
           niet het gewenste resultaat oplevert.
           @interaction[(sci-exponent 1000)]
           We krijgen @scheme[2] als resultaat, maar verwachten eigenlijk @scheme[3].
           Dit komt omdat computers kommagetallen niet exact kunnen voorstellen.
           @interaction[(/ (log 1000) (log 10))]
           We zien hier een eerste keer een voorbeeld van een afrondingsfout door de scheme-interpreter.}


@interaction[(sci-coefficient 7000)
             (sci-exponent 7000)]
