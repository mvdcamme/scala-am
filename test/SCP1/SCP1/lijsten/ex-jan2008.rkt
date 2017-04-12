#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica januari 2008: Lijst vraag}
Het is solden in de winkelketen Z&M. Bij aankoop van een jas krijg je 50% korting, bij aankoop van een kleedje 50%, bij aankoop van een rok 30% en bij aankoop van een trui 20% en voor al de rest 25%. De ontwikkelaar die de nodige software voor het kassasysteem van de winkelketen moet schrijven, stelt deze kortingen als volgt voor
@scheme['((jas 50) (kleed 50) (rok 30) (trui 20))].

De aankoop van een bepaalde klant wordt eveneens in een geneste lijststructuur voorgesteld.
Bvb. Jan koopt een jas van 100 euro, een trui van 25 euro, een rok van 70 euro en een T-shirt van 20 euro: 
@scheme['((jas 100) (trui 25) (rok 70) (t-shirt 20))].

@section[#:tag "vraag1"]{Bereken totaal}
Schrijf nu een procedure @scheme[(totaal aankoop korting)] die voor de aankoop van een klant het totale bedrag berekent. 

@solution{@def+int[(define (totaal aankopen kortingen) 
                     
                     (define (zoek-korting kortingen artikel)
                       (apply + 
                              (map 
                               (lambda (x) (if (eq? (car x) artikel) 
                                               (cadr x) 
                                               0)) 
                               kortingen)))
                     
                     (if (null? aankopen) 
                         0
                         (let* ((aankoop (car aankopen))
                                (korting (zoek-korting kortingen (car aankoop)))
                                (prijs (cadr aankoop)))
                           (+ (- prijs (/ (* prijs korting) 100))
                              (totaal (cdr aankopen) (cdr kortingen))))))]}

@interaction[(totaal '((jas 100) (trui 25) (rok 70) (t-shirt 20))
                     '((jas 50) (kleed 50) (rok 30) (trui 20)))]


@section{Recursie/iteratie}
Genereren de procedures in je oplossing recursieve of iteratieve processen? Herschrijf je oplossing uit @secref{vraag1} zodat je procedures processen van het andere type genereren.

@solution{@def+int[(define (totaal-iter aankopen kortingen)
                     
                     (define (zoek-korting kortingen artikel)
                       (apply + (map 
                                 (lambda (x) (if (eq? (car x) artikel) 
                                                 (cadr x) 
                                                 0)) 
                                 kortingen)))
                     
                     (define (loop lst res)
                       (if (null? lst)
                           res
                           (let* ((aankoop (car lst))
                                  (korting (zoek-korting kortingen (car aankoop)))
                                  (prijs (cadr aankoop)))
                             (loop (cdr lst) 
                                   (+ res (- prijs 
                                             (/ (* prijs korting) 100)))))))
                     (loop aankopen 0))]}

@interaction[(totaal-iter '((jas 100) (trui 25) (rok 70) (t-shirt 20))
                          '((jas 50) (kleed 50) (rok 30) (trui 20)))]



@section{Herschrijf procedure totaal}
Veronderstel dat er reeds een procedure @scheme[korting] bestaat die gegeven een artikel en zijn waarde, de korting voor dit artikel teruggeeft. Herschrijf de procedure @scheme[totaal] uit @secref{vraag1} zodanig dat de parameter @scheme[korting] in @scheme[(totaal aankoop korting)] die procedure bevat.

@solution{@def+int[(define (totaal aankopen korting)
                     (if (null? aankopen) 
                         0
                         (let* ((aankoop (car aankopen))
                                (k (korting (car aankoop)))
                                (prijs (cadr aankoop)))
                           (+ (- prijs (/ (* prijs k) 100))
                              (totaal (cdr aankopen) korting)))))]}


@section{Korting}
Implementeer ook de procedure @scheme[korting] voor de winkelketen Z&M (m.a.w. bij aankoop van een jas rijg je 50% korting, bij aankoop van een kleedje 50%, bij aankoop van een rok 30%, bij aankoop van een trui 20% en voor al de rest 25%).

@solution{@defs+int[((define Z&Mkortingen '((jas 50) (kleed 50) (rok 30) (trui 20)))
                     
                     (define (korting artikel)
                       (apply + 
                              (map 
                               (lambda (x) (if (eq? (car x) artikel) 
                                               (cadr x) 
                                               0)) 
                               Z&Mkortingen))))]}

@interaction[(totaal '((jas 100) (trui 25) (rok 70) (t-shirt 20)) korting)]

           
           
           
           