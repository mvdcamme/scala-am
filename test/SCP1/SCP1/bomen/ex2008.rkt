#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica augustus 2008}
Bedrijven zoals Coca-Cola, Inbev, etc. hebben een uitgebreid productengamma
dat onderverdeeld is in verschillende categorieën en subcategorieën die elk
verschillende producten bevatten. Elk product haalt een bepaald omzetcijfer.
Bvb. beschouw de volgende boom van het bedrijf Coca-Cola (die een gelimiteerde
versie van het volledige gamma voorstelt).

@image["bomen/images/coca-cola.png" #:scale 0.125]

@def+int[(define Coca-Cola-NV 
           '(Coca-Cola-NV (Frisdranken 
                           (Coca-Cola 
                            (Regular-Coca-Cola (Coke (10000000)))
                            (light-Coca-Cola (Coke-Light (800000))
                                             (Coke-Zero (200000))))
                           (Fanta (Fanta-Orange (800000))
                                  (Fanta-Lemon (200000)))
                           (Sprite (Sprite-Zero (1000000))))
                          (Sappen 
                           (Minute-Maid (Minute-Maid-Sinaas (2000000))
                                        (Minute-Maid-Tomaat (1000000))))))]

@solution{@defs+int[((define (omzetcijfer categorie)
                       (caadr categorie))
                     
                     (define (heeft-omzetcijfer categorie)
                       (and (pair? categorie)
                            (pair? (cadr categorie))
                            (atom? (caadr categorie))
                            (number? (caadr categorie))))
                     
                     (define (deel-categorien categorie)
                       (cdr categorie))
                     
                     (define (hoofdcategorie categorie)
                       (car categorie)))]}


@section{Bereken omzet}
Schrijf een procedure @scheme[(omzet bedrijf categorie)] die de totale omzet
van een gegeven categorie of product binnen het bedrijf teruggeeft.

@solution{@defs+int[((define (bereken lst)
                       (cond ((null? lst) 0)
                             ((atom? lst) 0)
                             ((number? (car lst)) (car lst))
                             (else (+ (bereken (car lst))
                                      (bereken (cdr lst))))))
                     
                     (define (omzet bedrijf categorie)
                       (if (eq? (hoofdcategorie bedrijf) categorie)
                           (bereken bedrijf)
                           (omzet-in (deel-categorien bedrijf) categorie)))
                     
                     (define (omzet-in lst categorie)
                       (if (null? lst)
                           #f
                           (or (omzet (car lst) categorie)
                               (omzet-in (cdr lst) categorie)))))]}

@interaction[(omzet Coca-Cola-NV 'Coca-Cola)]
@interaction[(omzet Coca-Cola-NV 'Sprite)]
@interaction[(omzet Coca-Cola-NV 'Minute-Maid)]


@section{Verdeel budget democratisch}
De dienst marketing van zo een bedrijf heeft een bepaald budget dat op
verschillende manieren kan verdeeld worden over producten. Een eerste manier
is om het gegeven marketingbudget evenredig met het omzetcijfer te verdelen
over de producten.

Schrijf een procedure @scheme[(verdeel-democratisch bedrijf budget)] die een
lijstje teruggeeft met hoeveel budget elk product van het bedrijf krijgt
afhankelijk van zijn omzetcijfer.

@solution{@defs+int[((define (collect-pairs bedrijf)  
                       (cond ((heeft-omzetcijfer bedrijf)
                              (list (list (hoofdcategorie bedrijf) 
                                          (omzetcijfer bedrijf))))
                             (else (collect-pairs-in (deel-categorien bedrijf)))))
                     
                     (define (collect-pairs-in lst)
                       (if (null? lst)
                           '()
                           (append (collect-pairs (car lst))
                                   (collect-pairs-in (cdr lst)))))
                     
                     (define (verdeel-democratisch bedrijf budget)
                       (let* ((pairs (collect-pairs bedrijf))
                              (total (apply + (map cadr pairs)))
                              (factor (/ budget total)))
                         (map (lambda (x) (list (car x) (* factor (cadr x)))) 
                              pairs))))]}

@interaction[(verdeel-democratisch Coca-Cola-NV 128000000)]


@section{Onafhankelijke verdeling van het budget}
Een andere strategie is om het budget per afzonderlijk product toe te kennen
onafhankelijk van het omzetzijfer van het product. Deze verdeling gebeurt dan
volgens hetvolgende principe: het algemene marketingbudget wordt gelijk
verdeeld over het aantal productcategorieën, het budget van elke categorie
wordt terug gelijk verdeeld over die categorie zijn subcategorieën, enz.

Schrijf een procedure @scheme[(verdeel bedrijf budget)] die gegeven een
bedrijfsboom en een budget de budgetboom teruggeeft waarbij de bladeren van de
boom niet meer het omzetcijfer bevatten, maar het marketingbudget dat elk
product volgens dit principe krijgt.

@solution{@defs+int[((define (verdeel bedrijf budget)
                       (if (heeft-omzetcijfer bedrijf)
                           (list (hoofdcategorie bedrijf) budget)
                           (let* ((rest (deel-categorien bedrijf))
                                  (new-budget (/ budget (length rest))))
                             (cons (hoofdcategorie bedrijf) 
                                   (verdeel-in rest new-budget)))))
                     
                     (define (verdeel-in lst budget)
                       (if (null? lst)
                           '()
                           (cons (verdeel (car lst) budget)
                                 (verdeel-in (cdr lst) budget)))))]}

@interaction[(verdeel Coca-Cola-NV 1200000)]


