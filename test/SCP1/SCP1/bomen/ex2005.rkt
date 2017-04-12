#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica januari 2005}
De beginnende circusgroep "Het grote VUB-circus" oefent volop met het bouwen van levende piramides. Een piramide kan gebouwd worden doordat 1 of meerdere artiesten een andere artiest optillen. Op zijn beurt kan de opgetilde artiest (eventueel samen met andere opgetilde artiesten op hetzelfde niveau) een andere artiest optillen.
Omwille van de veiligheid kan een artiest slechts 1 artiest optillen. Onrechtstreeks steunt hij echter alle artiesten die rechtstreeks en onrechtstreeks ondersteund worden door de artiest die hij zelf mee optilt. We kunnen deze piramides voorstellen door bomen, zoals bijvoorbeeld in onderstaande figuur. De kinderen van een knoop zijn al de artiesten die de artiest in de gegeven knoop optillen.

@image["bomen/images/piramide.png" #:scale 0.1375]

We kunnen deze piramides in Scheme voor stellen door geneste lijsten. De geneste lijst representatie voor deze specifieke piramide is de volgende:

@def+int[(define VUB-circus '(ann (mien (eef (bas)
                                             (bob))
                                        (els (jan)
                                             (jos))
                                        (eva (tom)
                                             (tim)))
                                  (mies (ine (cas) 
                                             (cor))
                                        (ils (rik)
                                             (raf))
                                        (ines (stef)
                                              (staf)))))]

@solution{@defs+int[((define (hoofdartiest piramide) (car piramide))
                     (define (artiesten piramide) (cdr piramide))
                     (define (artiest? piramide) 
                       (and (pair? piramide) (atom? (car piramide))))
                     (define (onderaan? piramide) (null? (cdr piramide))))]}


Gegeven deze representatie, schrijf de volgende piramide bewerkingen op in Scheme:

@section{Laat ondersteunde artiesten springen}
Simuleer het feit dat een artiest zijn last niet meer kan dragen: Wanneer een artiest het te moeilijk krijft om zijn last te dragen, roept hij alle artiesten die hij rechtstreeks en onrechtstreeks steunt dat ze eraf moeten springen. Zelf doet hij dit niet. Met andere woorden, de artiest die hijzelf mee optilt moet springen alsook degene die door deze laatste mee wordt opgetild, enz. 

Schrijf een Scheme functie @scheme[jump], die een piramide en de naam van de artiest die roept als input neemt en die de namen van de artiesten die hierbij moeten springen teruggeeft in een lijstje.

@solution{@defs+int[((define (jump piramide artiest)
                       (define (jump-hulp piramide pad)
                         (if (and (artiest? piramide)
                                  (eq? (hoofdartiest piramide) artiest)) 
                             pad
                             (jump-in (artiesten piramide)
                                      (cons (hoofdartiest piramide) pad))))
                       
                       (define (jump-in lst pad)
                         (if (null? lst)
                             #f
                             (or (jump-hulp (car lst) pad)
                                 (jump-in   (cdr lst) pad))))
                       (reverse (jump-hulp piramide '()))))]}

@interaction[(jump VUB-circus 'eva)]
@interaction[(jump VUB-circus 'stef)]


@section{Laat een artiest uit de piramide vallen}
Simuleer een val van een artiest: Wanneer een artiest zodanig uit evenwicht geraakt dat hij uit de piramide vakt, zullen ook al de artiesten die hij rechtstreeks en onrechtstreeks ondersteunt meevallen, alsook alle artiesten die hem rechtstreeks dragen (dus alle artiesten van 1 niveau lager die hem rechtstreeks optillen, en niiet degene daaronder). 

Schrijf een procedure @scheme[fall], die een piramide en de naam van de artiest die valt als input neemt, en die de namen van alle artiesten die vallen teruggeeft in een lijstje.

@solution{@defs+int[((define (fall piramide artiest)
                       (define (fall-hulp piramide pad)
                         (if (and (artiest? piramide)
                                  (eq? (hoofdartiest piramide) artiest)) 
                             (append pad 
                                     (list (hoofdartiest piramide))
                                     (map hoofdartiest (artiesten piramide))))
                         (fall-in (artiesten piramide) 
                                  (append pad 
                                          (list (hoofdartiest piramide)))))
                       
                       (define (fall-in lst pad)
                         (if (null? lst)
                             #f
                             (or (fall-hulp (car lst) pad)
                                 (fall-in (cdr lst) pad))))
                       (fall-hulp piramide '())))]}


@interaction[(fall VUB-circus 'eva)]
@interaction[(fall VUB-circus 'stef)]
@interaction[(fall VUB-circus 'mies)]

