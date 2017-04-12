#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica 1ste zit 1996}
Schrijf een @emph{destructieve} procedure @scheme[ontdubbel!] die een (platte) lijst van getallen als invoer neemt, en een cons-cel teruggeeft met als @scheme[car] de lijst van alle oneven elementen van de oorspronkelijke lijst, en als @scheme[cdr] de lijst van alle even elementen van de oorspronkelijke lijst. Bovendien is dit de @bold{enige}(!) nieuwe cons-cel die je procedure mag aanmaken. Let ook op dat de onderliggende volgorde der elementen behouden blijft.

@solution{@defs+int[((define (ontdubbel! lijst)
                       (let ((deEven '())
                             (deOneven '()))
                         (define (ontdubbel-iter prevE prevO restLijst)
                           (cond ((null? restLijst) (set-cdr! prevE '())
                                                    (set-cdr! prevO '())
                                                    (cons deEven deOneven))
                                 ((even? (car restLijst)) 
                                  (if (null? prevE)
                                      (set! deEven restLijst)
                                      (set-cdr! prevE restLijst))
                                  (ontdubbel-iter restLijst prevO (cdr restLijst)))
                                 (else (if (null? prevO)
                                           (set! deOneven restLijst)
                                           (set-cdr! prevO restLijst))
                                       (ontdubbel-iter prevE restLijst (cdr restLijst)))))
                         (ontdubbel-iter deEven deOneven lijst))))]}

@interaction[(ontdubbel! '(1 2 3 4 5 6 7 8 9 10))]