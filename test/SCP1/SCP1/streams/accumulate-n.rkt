#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@hidden-code[(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))]


@title{Accumulate-n}
Implementeer de procedure @scheme[(accumulate-n op ne streams)] waarbij @scheme[streams] een stroom van stromen is, allen met dezelfde lengte. @scheme[accumulate-n] zal @scheme[op] (met neutraal element @scheme[ne]) loslaten op de elementen van de eerste stroom uit @scheme[streams], vervolgens op de elementen van de tweede stroom uit @scheme[streams], enz. De resultaten worden in een nieuwe stroom teruggegeven.

@solution{@defs+int[((define (accumulate-n op ne streams)
                       (if (empty-stream? (head streams)) the-empty-stream
                           (let ((heads (map-stream head streams))
                                 (tails (map-stream tail streams)))
                             (cons-stream (accumulate op ne heads)
                                          (accumulate-n op ne tails))))))]}

@def+int[(define matrix 
           (cons-stream (enumerate-interval 1 3)
                        (cons-stream (enumerate-interval 4 6)
                                     (cons-stream (enumerate-interval 7 9)
                                                  (cons-stream (enumerate-interval 10 12)
                                                               the-empty-stream)))))]

@interaction[(print-stream matrix)]
@interaction[(print-stream (accumulate-n + 0 matrix))]