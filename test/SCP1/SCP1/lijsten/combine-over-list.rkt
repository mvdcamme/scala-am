#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Combine over list}
De meeste Scheme-versies bevatten een voorgedefinieerde hogere-orde
procedure map over lijsten die als volgt kan geïmplementeerd worden:
@def+int[(define (map a-function a-list)
           (if (null? a-list)
               '()
               (cons (a-function (car a-list))
                     (map a-function (cdr a-list)))))]

Analoog met map kan je ook een hogere-procedure combine-over-list op lijsten als volgt definiëren:
@def+int[(define (combine-over-list op unity a-list)
           (if (null? a-list)
               unity
               (op (car a-list)
                   (combine-over-list op unity (cdr a-list)))))]


Gebruik nu @scheme[map] en/of @scheme[combine-over-list] om:
@itemize{
@item{van een lijst van getallen de som van de kwadraten te berekenen.
@solution[@defs+int[((define (square x) (* x x))
                     
                     (define (som-van-de-kwadraten list)
                       (combine-over-list + 0 (map square list))))]]}


@item{te testen of een lijst uitsluitend oneven getallen omvat.
@solution[@defs+int[((define (my-and a b)
                       (and a b))
                     
                     (define (uitsluitend-oneven? list)
                       (combine-over-list my-and #true (map odd? list))))]]}
}
@(reset-r5rs-evaluator)

