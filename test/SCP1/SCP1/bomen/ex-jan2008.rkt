#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Examen Informatica januari 2008: Boom vraag}
Traditiegetrouw geven (bet)(over)grootouders met Nieuwjaar nieuwjaarsgeld aan
hun nakomelingen. Veronderstel dat de nakomelingen in Scheme voorgesteld
worden als een familieboom. Bvb. beschouw de volgende familieboom, waarbij Jan
de grootouder is en nieuwjaarsgeld uitdeelt aan zijn nakomelingen. Dit kan
echter op verschillende manieren gebeuren.

@image["bomen/images/ex2008.png" #:scale 0.20]

@def+int[(define familieboom '(jan (piet (frans (tom)
                                                (roel))
                                         (mie))
                                   (bram (inge (bert (ina)
                                                     (ilse))
                                               (bart))
                                         (iris))
                                   (joost (else (ilse)))))]

@solution{@defs+int[((define (familiehoofd fam) (car fam))
                     (define (kinderen fam) (cdr fam))
                     (define (laatste-nakomeling? fam) 
                       (null? (kinderen fam))))]}

@section{Verdeel democratisch}
Gegeven een budget, beslist een grootouder om iedereen evenveel geld te geven.
Schrijf een procedure @scheme[verdeel-democratisch] die gegeven een
familieboom en een budget, berekent hoeveel elk familielid krijgt.

@solution{@defs+int[((define (verdeel-democratisch boom budget)
                       (define (verdeel boom)
                         (if (laatste-nakomeling? boom)
                             1
                             (+ 1 (verdeel-in (kinderen boom)))))
                       
                       (define (verdeel-in lst)
                         (if (null? lst)
                             0
                             (+ (verdeel (car lst))
                                (verdeel-in (cdr lst)))))
                       (/ budget (verdeel-in (kinderen boom)))))]}

@interaction[(verdeel-democratisch familieboom 1500)]


@section{Bereken budget}
Andere grootouders beslissen om aan hun kinderen elk 100 euro te geven, aan
hun kleinkinderen 50 euro en aan hun achterkleinkinderen 20 euro. Schrijf een
procedure @scheme[budget] die het budget dat de grootouders nodig hebben,
berekent.

@solution{@defs+int[((define (budget boom budget-list)
                       (define (budget-hulp boom budget-list)
                         (+ (car budget-list)
                            (budget-hulp-in (kinderen boom) (cdr budget-list))))
                       
                       (define (budget-hulp-in bomen budget-list)
                         (if (null? bomen)
                             0
                             (+ (budget-hulp    (car bomen) budget-list)
                                (budget-hulp-in (cdr bomen) budget-list))))
                       (budget-hulp-in (kinderen boom) budget-list)))]}


@interaction[(budget familieboom '(100 50 20))]



@section{Verdeel budget onder nakomelingen zonder kinderen}
Nog andere grootouders beslissen om enkel aan de nakomelingen die geen
kinderen hebben nieuwjaarsgeld te geven. Ze doen dit volgens het volgende
principe: hun budget wordt gelijk verdeeld over hun aantal kinderen, het
budget van elk kind wordt terug gelijk verdeeld over dat kind zijn kinderen,
enz. Schrijf een procedure @scheme[verdeel] die, gegeven een familieboom en
een budget, een lijstje teruggeeft waarvan elk element terug een lijstje is
met de naam van de nakomeling (die zelf geen kinderen meer heeft) en het
bedrag dat deze krijgt.

@solution{@defs+int[((define (verdeel boom budget)
                       (cond ((laatste-nakomeling? boom)
                              (list (list (familiehoofd boom) budget)))
                             (else (let* ((rest (kinderen boom))
                                          (new-budget (/ budget (length rest))))
                                     (verdeel-in rest new-budget)))))
                     
                     (define (verdeel-in bomen budget)
                       (if (null? bomen)
                           '()
                           (append (verdeel    (car bomen) budget)
                                   (verdeel-in (cdr bomen) budget)))))]}


@interaction[(verdeel familieboom 3000)]
