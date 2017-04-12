#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen Informatica Partieel januari 1995}
Frank is dolgelukkig. Hij is er zopas in geslaagd zijn eerste "object met toestand" in Scheme aan de praat te krijgen. Hij heeft meer bepaald een @scheme[counter] object gemaakt dat reageert op de boodschappen @scheme[reset] (op 0 zetten), @scheme[next] (1 ophogen) en @scheme[read] (waarde uitlezen).

Frank's volgende opdracht is nu om een object @scheme[scorebord] te maken voor de plaatselijke basketbalvereniging. Dit is een object dat de score bijhoudt van de thuisploeg en de bezoekersploeg en dat slechts reageert op de boodschappen @scheme[reset] (beide scores op 0 zetten), @scheme[read] (beide scores uitlezen) en @scheme[score] (de score van 1 van de 2 ploegen verhogen met 1, 2 of 3 punten).

Frank is echter zo fier op zijn eerste creatie dat hij ze kost wat kost wil hergebruiken. I.h.b. wil hij dus proberen om binnenin zijn @scheme[scorebord] twee @scheme[counter] objecten te gebruiken die de score van elk van de ploegen bijhouden.

@section{Implementeer}
Schrijf het geheel uit. Dus eerst een simpel @scheme[counter] object, daarna een @scheme[scorebord] object dat binnenin twee @scheme[counter] objecten gebruikt.

@solution{@defs+int[((define (create-counter)
                       (let ((value 0))
                         
                         (define (reset)
                           (set! value 0)
                           'ok)
                         
                         (define (next)
                           (set! value (+ 1 value))
                           'ok)
                         
                         (define (increase x)
                           (set! value (+ value x)))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'reset) reset)
                                 ((eq? msg 'next) next)
                                 ((eq? msg 'read) value)
                                 ((eq? msg 'increase) increase)
                                 (else (error "wrong message: " msg))))
                         dispatch))
                     
                     
                     (define (make-scorebord)
                       (let ((c-home (create-counter))
                             (c-visit (create-counter)))
                         
                         (define (reset)
                           ((c-home 'reset))
                           ((c-visit 'reset))
                           'ok)
                         
                         (define (read)
                           (let ((c1 (c-home 'read))
                                 (c2 (c-visit 'read)))
                             (display c1)
                             (display "-")
                             (display c2)
                             (newline)
                             'ok))
                         
                         (define (score team n)
                           (cond ((not (or (= n 1) (= n 2) (= n 3)))
                                  (newline)
                                  (display "De score kan slechts 1, 2 of 3 zijn!")
                                  (newline)
                                  'ok)
                                 ((eq? team 'home) 
                                  ((c-home 'increase) n)
                                  'ok)
                                 ((eq? team 'visit)
                                  ((c-visit 'increase) x)
                                  'ok)
                                 (else (error "wrong team: " team))))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'reset) reset)
                                 ((eq? msg 'read) read)
                                 ((eq? msg 'score) score)
                                 (else (error "wrong message: " m))))
                         dispatch)))]}


@section{Manipuleer}
Laat zien hoe je een @scheme[scorebord] object aanmaakt en hoe je het manipuleert.

@solution{@itemize{@item[@interaction[(define bord (make-scorebord))]]
                   @item[@interaction[((bord 'read))]]
                   @item[@interaction[((bord 'score) 'home 2)  ]]
                   @item[@interaction[((bord 'read))]]
                   @item[@interaction[((bord 'score) 'visit 5)]]
                   @item[@interaction[((bord 'read))]]
                   @item[@interaction[((bord 'reset))]]
                   @item[@interaction[((bord 'read))]]}}
