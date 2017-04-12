#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@hidden-code[(define (print-m matrix)
               (display "[")
               (print-stream (head matrix)) (newline)
               (stream-for-each (lambda (x) (display " ")(print-stream x) (newline)) (tail matrix))
               (display "]\n"))]


@title{Examen Informatica eerste zit 1997}
@section{Cut}
Schrijf een procedure @scheme[(cut stream)] die een stroom van getallen, gesorteerd in stijgende volgorde en waarin elk getal meerdere malen kan voorkomen, als invoer neemt en deze omvormt in een stroom van stromen, waarbij elke deelstroom bestaat uit alle voorkomens van 1 enkel getal. Zie figuur ter verduidelijking.

@image["streams/images/ex1997-1.png" #:scale 0.20]

@solution{@defs+int[((define (cut stream)
                       (define (split head-stream stream e)
                         (if (empty-stream? stream)
                             (cons head-stream the-empty-stream)
                             (if (not (= e (head stream)))
                                 (cons head-stream stream)
                                 (split (cons-stream e head-stream) (tail stream) e))))
                       
                       (if (empty-stream? stream)
                           the-empty-stream
                           (let ((temp (split the-empty-stream stream (head stream))))
                             (cons-stream (car temp) (cut (cdr temp)))))))]}

@interaction[(print-m (cut (list->stream '(1 1 1 2 2 3 5 5 6 6 6))))]


@section{Merge}
Schrijf een procedure @scheme[(merge str1 str2)] die twee gesorteerde stromen als invoer neemt, en er een nieuwe gesorteerde stroom van maakt. Het merge-algoritme mag echter @bold{geen} dubbels verwijderen!

@image["streams/images/ex1997-2.png" #:scale 0.20]

@solution{@defs+int[((define (merge s1 s2)
                       (cond
                         ((empty-stream? s1) s2)
                         ((empty-stream? s2) s1)
                         ((> (head s1) (head s2)) (merge s2 s1))
                         (else (cons-stream (head s1) (merge (tail s1) s2))))))]}

@interaction[(print-stream (merge (list->stream '(1 4 5 6 8 9))
                                  (list->stream '(1 2 2 3 5 7 7 9))))]

@section{Merge-n}
Maak nu gebruik van deze procedure @scheme[merge] om eem procedure @scheme[merge-n] te definiëren die een @bold{eindige} stroom van gesorteerde stromen als invoer neemt en deze sorteert door stapsgewijs de deelstroom te combineren met de procedure @scheme[merge].

@image["streams/images/ex1997-3.png" #:scale 0.20]

@solution{@defs+int[((define (merge-n streams)
                       (if (empty-stream? (tail streams))
                           (head streams)
                           (merge (head streams) (merge-n (tail streams))))))]}

@interaction[(print-stream (merge-n (list->stream
                                     (list
                                      (list->stream '(1 1 1 2  2 3 4  4 5 6 6 7))
                                      (list->stream '( 1 2 2 2 3 4 4 4 5 6 6 7))
                                      (list->stream '(3 3 4  4 5 5 6 6 7))))))]


@section{Traffiek in een pretpark}
In een pretpark zijn de ingangen afgezet met pportjes om slechts 1 persoon (per poortje) tegelijk door te laten. Veronderstel nu dat elk poortje, op het moment dat er persoon passeert, het huidige tijdstip (uur) op een stroom zet. Zo'n stroom ziet er dan bvb. als volgt uit (3 personen om 9 uur, 4 personen om 10 uur, 2 personen om 11 uur, enz.):

@image["streams/images/ex1997-5.png" #:scale 0.20]

Om nu te weten te komen op welke uren het pretpark de meeste bezoekers ontvangt wensen we deze gegevens te analyseren. We plaatsen daartoe alle stromen van tijstippen (voor elk poortje 1 stroom) op 1 grote (maar eindige) stroom van stromen, en geven deze als input aan een procedure @scheme[pretpark-traffiek]. Deze procedure genereert een stroom van koppeltjes bestaande uit een tijdstip, en het totaal aantal bezoekers op dat tijdstip. Voorbeeld:

@image["streams/images/ex1997-4.png" #:scale 0.20]

Maak gebruik van de procedures @scheme[cut] en @scheme[merge-n] om de procedure @scheme[pretpark-traffiek] te definiëren.

@solution{@defs+int[((define (pretperk-traffiek stream)
                       (map-stream (lambda (x)
                                     (cons (head x)
                                           (accumulate + 
                                                       0 
                                                       (map-stream (lambda (x) 1) x))))
                                   (cut (merge-n stream)))))]}

@interaction[(print-stream 
              (pretperk-traffiek (list->stream
                                  (list
                                   (list->stream '(1 1 1 2  2 3 4  4 5 6 6 7))
                                   (list->stream '( 1 2 2 2 3 4 4 4 5 6 6 7))
                                   (list->stream '(3 3 4  4 5 5 6 6 7))))))]