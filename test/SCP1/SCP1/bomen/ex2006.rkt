#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica augustus 2006}
Na lang onderzoek en vele experimenten zijn landbouwers erin geslaagd om hybride fruitbomen te kweken. Onderstaand schema toont een tak uit een hybride appel/peer boom. In dit geval kan de tak een willekeurig aantal keer vertakken in nieuwe takken, bladeren, appelen of peren.

Het is wel zo dat in een normale hybride tak, een tak niet rechtstreeks kan vertakken in fruit van verschillende types. De rechtstreekse kinderen van een tak zijn dus in de hybride appel/peer boom ofwel appelen, blaadjes en takken; ofwel peren, blaadjes en takken. Onderstaand voorbeekd is een normale hybride tak.

@image["bomen/images/hybridtree.png" #:scale 0.07]

@section{Abstract Data Type}
Schrijf een ADT @scheme[hybride-tak] dat toelaat om dit type boom voor te stellen en te gebruiken in Scheme.

@solution{@defs+int[((define (maak-blad type) type)
                     (define (geef-type blad) blad)
                     
                     (define (maak-knoop deelbomen) deelbomen)
                     (define (geef-deelbomen boom) boom)
                     
                     (define (maak-hybride-tak knopen) knopen)
                     (define (geef-knopen tak) tak)
                     
                     (define (leeg? boom) (null? boom))
                     (define (knoop? boom) (pair? boom))
                     (define (blad? boom) (atom? boom))
                     
                     (define hybride-tak 
                       (maak-hybride-tak
                        (list 
                         (maak-knoop 
                          (list 
                           (maak-knoop (list (maak-blad 'appel)
                                             (maak-blad 'appel)
                                             (maak-blad 'blad)))
                           (maak-blad 'peer)))
                         (maak-knoop (list (maak-blad 'blad)
                                           (maak-blad 'peer)))
                         (maak-knoop (list (maak-blad 'appel)
                                           (maak-knoop (list (maak-blad 'appel)
                                                             (maak-blad 'blad))))))))
                     
                     (define tak 
                       (maak-hybride-tak
                        (list 
                         (maak-knoop 
                          (list (maak-knoop (list (maak-blad 'appel)
                                                  (maak-blad 'appel)
                                                  (maak-blad 'blad)))
                                (maak-blad 'peer)))
                         (maak-knoop (list (maak-blad 'blad)
                                           (maak-blad 'peer)
                                           (maak-blad 'appel)))
                         (maak-knoop (list (maak-blad 'appel)
                                           (maak-knoop (list (maak-blad 'appel)
                                                             (maak-blad 'blad)))))))))]}


@section{Tel fruit en bladeren}
Schrijf een functie @scheme[tel] die gegeven een hybride appel/peer tak, teruggeeft hoeveel appelen, peren en blaadjes aan de tak hangen.

@solution{@defs+int[((define (tel boom)    
                       (define (combine-results l1 l2)
                         (list (+ (car l1) (car l2))
                               (+ (cadr l1) (cadr l2))
                               (+ (caddr l1) (caddr l2))))
                       
                       (define (tel-hulp boom)
                         (cond ((leeg? boom) (list 0 0 0))
                               ((and (blad? boom) (eq? boom 'appel)) 
                                (list 1 0 0))
                               ((and (blad? boom) (eq? boom 'peer)) 
                                (list 0 1 0))
                               ((blad? boom) (list 0 0 1))
                               (else (tel-hulp-in (geef-knopen boom)))))
                       
                       (define (tel-hulp-in lst)
                         (if (null? lst)
                             (list 0 0 0)
                             (combine-results (tel-hulp (car lst))
                                              (tel-hulp-in (cdr lst)))))
                       (tel-hulp boom)))]}

@interaction[(tel hybride-tak)]


@section{Normale tak?}
Schrijf een functie @scheme[normaal?] die nagaat of een gegeven hybride appel/peer tak normaal is, m.a.w. die nagaat of er geen knoop bestaat die zowel een appel als een peer als rechstreekse kinderen heeft.

@solution{@defs+int[((define (member? x lst)
                       (pair? (memq x lst)))
                     
                     (define (normaal? knoop)
                       (let ((types (map (lambda (x) (if (pair? x) 'tak x)) knoop)))
                         (not (and (member? 'appel types) (member? 'peer types)))))
                     
                     (define (check-normaal boom)
                       (cond ((leeg? boom) #t)
                             ((blad? boom) #t)
                             ((knoop? boom) 
                              (and (normaal? boom) 
                                   (check-normaal-in (geef-knopen boom))))
                             (else (check-normaal-in (geef-knopen boom)))))
                     
                     (define (check-normaal-in lst)
                       (if (null? lst)
                           #t
                           (and (check-normaal (car lst))
                                (check-normaal-in (cdr lst))))))]}

@interaction[(check-normaal hybride-tak)]
