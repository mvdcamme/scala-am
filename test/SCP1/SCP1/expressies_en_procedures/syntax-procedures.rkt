#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math @; put this ine ach file that uses formula's!

@title{Syntax van procedures}

@section{fourth}
Schrijf een procedure @scheme[fourth] die de vierde macht van zijn 
argument berekent.
Doe dit op twee verschillende manieren,
eerst eens door gebruik te maken van de vermenigvuldiging
en dan eens door @scheme[square] te gebruiken.

@def+int[(define (square x) (* x x))]

@solution[@def+int[(define (fourth-* x)
                     (* x x x x))]]

@solution[@def+int[(define (fourth-square x)
                     (square (square x)))]]


@section{sum-3-squares}
Schrijf een procedure @scheme[sum-3-squares] die drie getallen als 
argumenten neemt en de som van hun kwadraten teruggeeft.
@solution[@def+int[(define (sum-3-squares a b c)
                     (+ (square a) (square b) (square c)))]]


@section{Celsius naar Fahrenheit}
Schrijf een procedure @scheme[convert-C-to-F] die een temperatuur in 
graden Celsius omzet in graden Fahrenheit.
Gebruik hiervoor de formule: @math-in{F = (C + 40)\times 1.8 - 40}.

@solution[@def+int[(define (convert-C-to-F c)
                     (- (* (+ c 40) 1.8) 40))]]


@section{Fahrenheit naar Celsius}
Schrijf de inverse procedure @scheme[convert-F-to-C] die een 
temperatuur in graden Fahrenheit omzet in graden Celsius.
@solution[@def+int[(define (convert-F-to-C f)
                     (- (/ (+ f 40) 1.8) 40))]]


@section{oppervlakte en omtrek}
Schrijf procedures die de oppervlakte en omtrek van volgende figuren 
berekenen: driehoek, vierkant, en cirkel.
@solution[@def+int[(define (oppervlakte-driehoek basis hoogte)
                     (/ (* basis hoogte) 2))
                   
                   (define (omtrek-driehoek zijde1 zijde2 zijde3)
                     (+ zijde1 zijde2 zijde3))
                   
                   (define (oppervlakte-vierkant zijde)
                     (* zijde zijde))
                   
                   (define (omtrek-vierkant zijde)
                     (* zijde 4))
                   
                   (define pi 3.14)
                   (define (oppervlakte-cirkel r)
                     (* pi r r))
                   
                   (define (omtrek-cirkel r)
                     (* 2 pi r))]]



@section{oppervlakte en inhoud}
Doe hetzelfde voor oppervlakte en inhoud van enkele drie-dimensionale
figuren zoals een balk, een bol en een cilinder.
@solution[@defs+int[((define (inhoud-balk lengte breedte hoogte)
                       (* lengte breedte hoogte))
                     
                     (define (oppervlakte-balk lengte breedte hoogte)
                       (+ (* lengte breedte 2) 
                          (* lengte hoogte  2)
                          (* breedte hoogte 2)))
                     
                     (define (inhoud-bol r)
                       (* 4/3 pi (expt r 3)))
                     
                     (define (oppervlakte-bol r)
                       (* 4 pi r r))
                     
                     (define (inhoud-cilinder h r)
                       (* (oppervlakte-cirkel r) h))
                     
                     (define (oppervlakte-cilinder h r)
                       (+ (* (oppervlakte-cirkel r) 2)
                          (* (omtrek-cirkel r) h))))]]

