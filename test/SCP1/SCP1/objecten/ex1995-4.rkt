#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Wiskunde Partieel januari 1995}
Eufrasie is dolgelukkig. Ze is er net in geslaagd haar eerste "object met toestand" in Scheme aan de praat te krijgen. Ze heeft meer bepaald een @scheme[counter] object gemaakt dat reageert op de boodschappen @scheme[increase] (1 ophogen), @scheme[decrease] (1 verlagen) en @scheme[read] (waarde uitlezen). Haar code is correct en ziet er als volgt uit:

@def+int[(define (create-counter initial)
           (define (increase) (set! initial (+ initial 1)))
           (define (decrease) (set! initial (- initial 1)))
           (define (read) initial)
           (define (dispatch m)
             (cond ((eq? m 'increase) (increase))
                   ((eq? m 'decrease) (decrease))
                   ((eq? m 'read) (read))
                   (else (display "wrong message"))))
           dispatch)]

Nu wil Eufrasie een @scheme[counter] object gebruiken om een simulatie van een parking met 2 verdiepen te implementeren. Elk verdiep heeft een zekere capaciteit. De verdiepen worden in volgorde opgevuld, m.a.w. als een bepaald verdiep vol zit (leeg is) moet naar het bovenliggende (onderliggende) verdiep (indien er nog een is) worden overgegaan. Zo'n parkeergarage wordt als volgt aangemaakt: 
@scheme[(define parkeergarage (create-parking capacity1 capacity2))]

en verstaat de volgende boodschappen:

@itemize{@item{@scheme[(parkeergarage 'full?)]: is de garage vol?}
          @item{@scheme[(parkeergarage 'empty?)]: is de garage leeg?}
          @item{@scheme[(parkeergarage 'level)]: geeft het nummer van het verdiep (1 of 2) dat momenteel wordt opgevuld}
          @item{@scheme[(parkeergarage 'car-enters)]: @scheme[#f] indien de garage vol is. In het andere geval wordt de teller van 1 der verdiepen met 1 vermeerderd.}
          @item{@scheme[(parkeergarage 'car-leaves)]: @scheme[#f] indien de garage leeg is. In het andere geval wordt de teller van 1 der verdiepen met 1 verminderd.}}

Implementeer @scheme[create-parking] die een parking aanmaakt waarbij de aantallen worden bijgehouden door @scheme[counter]-objecten zoals hierboven gedefinieerd (dus zonder de code uit @scheme[create-counter] opnieuw op te schrijven).

@solution{@defs+int[((define (create-parking . capaciteiten)
                       (let ((verdieping-ctrs (map create-counter capaciteiten))
                             (nr-verdiepingen (length capaciteiten))
                             (nbr-cars 0))
                         
                         (define (total-capacity)
                           (apply + capaciteiten))
                         
                         (define (full?)
                           (= nbr-cars (total-capacity)))
                         
                         (define (empty?)
                           (= nbr-cars 0))
                         
                         (define (max-reached-level level idx)
                           (>=  (level 'read) (list-ref capaciteiten (- idx 1))))
                         
                         (define (level-current)
                           (define (loop lst index)
                             (cond ((null? lst) #f)
                                   (else (let* ((level (car lst))
                                                (capacity (level 'read)))
                                           (if (> capacity 0)
                                               index
                                               (loop (cdr lst) (+ index 1)))))))
                           (loop verdieping-ctrs 1))
                         
                         (define (level-to-leave)
                           (define (loop lst index)
                             (cond ((null? lst) #f)
                                   (else (let* ((level (car lst))
                                                (capacity (level 'read)))
                                           (if (and (not (max-reached-level level index)) (>= capacity 0))
                                               index
                                               (loop (cdr lst) (- index 1)))))))
                           (loop (reverse verdieping-ctrs) nr-verdiepingen))
                         
                         (define (car-enters)
                           (let ((level (level-current)))
                             (if level
                                 (let ((verdieping-ctr (list-ref verdieping-ctrs
                                                                 (- level 1))))
                                   (set! nbr-cars (+ nbr-cars 1))
                                   (verdieping-ctr 'decrease))
                                 #f)))
                         
                         (define (car-leaves)
                           (let ((level (level-to-leave)))
                             
                             (if level
                                 (let ((verdieping-ctr (list-ref verdieping-ctrs (- level 1))))
                                   (set! nbr-cars (- nbr-cars 1))
                                   (verdieping-ctr 'increase))
                                 (let ((verdieping-ctr (list-ref verdieping-ctrs(- nr-verdiepingen 1))))
                                   (set! nbr-cars (- nbr-cars 1))
                                   (verdieping-ctr 'increase)))))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'full?) (full?))
                                 ((eq? msg 'empty?) (empty?))
                                 ((eq? msg 'level) (level-current))
                                 ((eq? msg 'car-enters) (car-enters))
                                 ((eq? msg 'lst) verdieping-ctrs)
                                 ((eq? msg 'car-leaves) (car-leaves))
                                 (else (error "wrong message"))))
                         dispatch)))]}

@itemize{@item[@interaction[(define parking (create-parking 3 5 2))]]
          @item[@interaction[(parking 'level)]]
          @item[@interaction[(parking 'full?)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'level)]]
          @item[@interaction[(parking 'empty?)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'full?) ]]
          @item[@interaction[(parking 'car-enters)]]
          @item[@interaction[(parking 'car-leaves)]]
          @item[@interaction[(parking 'car-leaves)]]
          @item[@interaction[(parking 'car-leaves)]]
          @item[@interaction[(parking 'level)]]}





