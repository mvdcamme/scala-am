#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (print-ring r)
               (define (aux l)
                 (if (not (null? l))
                     (if (eq? (cdr l) r)
                         (begin (display " ") (display (car l)) (display "..."))
                         (begin (display " ") (display (car l)) (aux (cdr l))))))
               (aux r)
               #t)]

@title{Examen Wiskunde 1ste zit 1994}
Schrijf een @emph{destructieve} procedure die een circulaire lijst omvormt tot een circulaire lijst met twee keer zoveel elementen, door vlak na ieder element van de lijst een nieuw element tussen te voegen waarvan de waarde gelijk is aan het kwadraat van het oorspronkelijke element. Bijvoorbeeld, de lijst met box-pointer diagram

@image["destructieve/images/box-pointer1.png" #:scale 0.15]

wordt omgevormd tot de volgende circulaire lijst:

@image["destructieve/images/box-pointer2.png" #:scale 0.15]

@solution{@defs+int[((define (kw-lijst lst)
                       (define (loop l)
                         (let ((rest (cdr l))
                               (n (list (* (car l) (car l)))))
                           (set-cdr! l n)
                           (set-cdr! n rest)
                           (if (not (eq? rest lst))
                               (loop rest))))
                       (loop lst)
                       lst))]}

@interaction[(define last-cons (cons 3 '()))]
@interaction[(define test-lst (cons 1 (cons 4 last-cons)))]
@interaction[(set-cdr! last-cons test-lst)]
@interaction[(print-ring (kw-lijst test-lst))]