#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen Informatica 1ste zit 1995}
Beschouw een lijst waarvan de elementen deellijsten zijn die 1 of meerdere elementen kunnen bevatten. Bijvoorbeeld: @scheme[((1 2) (3 4 5) (6) (7 8))].
Schrijf een @emph{destructieve} procedure @scheme[flatten!] die een dergelijke lijst als invoer neemt en er een vlakke lijst van maakt.

@bold{Let op}: aangezien het gaat om een destructieve procedure mag je geen enkele nieuwe cons-cel aanmaken. Je mag hoogstens bestaande cons-cellen wijzigen of weglaten.

@solution{@defs+int[((define (find-last lijst)
                       (if (null? lijst)
                           (error "find-last -- lijst heeft geen laatste element")
                           (let ((next (cdr lijst)))
                             (if (null? next)
                                 lijst
                                 (find-last next)))))
                     
                     (define (flatten! lijst)
                       (if (null? lijst)
                           '()
                           (let* ((sublist (car lijst))
                                  (restlist (flatten! (cdr lijst))))
                             (if (null? sublist)
                                 restlist
                                 (let ((last (find-last sublist)))
                                   (set-cdr! last restlist)
                                   sublist))))))]
           
           De gebruikte procedures zijn inderdaad destructief aangezien er nergens een
           nieuwe cons-cel (bvb. d.m.v. cons of list) wordt aangemaakt.
           
           Uitbreiding van de vraag: een destructieve flatten die op alle lijsten werkt
           we moeten hier wel een hulpcons-cel maken om een prev te kunnen bewaren
           
           @defs+int[((define (atom? x) (not (pair? x)))
                      
                      (define (flatten2! lijst)
                        (let ((hulpcel (cons 'dummy lijst)))
                          (define (flatten-aux! prev current)
                            (cond ((null? current) (cdr hulpcel))
                                  ((null? (car current)) 
                                   (set-cdr! prev (cdr current))
                                   (flatten-aux! prev (cdr current)))
                                  ((pair? (car current)) 
                                   (set-cdr! prev (flatten2! (car current)))
                                   (flatten-aux! (find-last prev) (cdr current)))
                                  ((null? (cdr prev)) 
                                   (set-cdr! prev current)
                                   (flatten-aux! (cdr prev) (cdr current)))
                                  ((atom? (car current)) 
                                   (flatten-aux! (cdr prev) (cdr current)))))
                          (flatten-aux! hulpcel lijst)
                          (cdr hulpcel))))]
           @interaction[(flatten2! '((1 (2 3) 4) 5 6 (7 8)))]
           @interaction[(flatten2! '((1 2) (3 4 5) (6) (7 8)))]
           @interaction[(flatten2! '(() (1 2) (3 4 5) () (6) (7 8)))]
           @interaction[(flatten2! '(1 2 (3 (4 5) 6 (7 8) 9) 10))]}


@interaction[(flatten! '((1 2) (3 4 5) (6) (7 8)))]
@interaction[(flatten! '(() (1 2) (3 4 5) () (6) (7 8)))]

