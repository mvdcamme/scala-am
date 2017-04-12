#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@title{Triples}
Schrijf een procedure @scheme[(odd-sum-triples max)] die een stream teruggeeft van alle lijsten van lengte 3 waarvan de eerste twee elementen oneven en kleiner dan @scheme[max] zijn en de som ervan gelijk is aan het derde element.

@solution{@defs+int[((define (odd-sum-tripples max)
                       (map-stream (lambda (x)
                                     (let ((l (car x))
                                           (r (cdr x)))
                                       (list l r (+ l r))))
                                   (let ((s (streamfilter odd? (enumerate-interval 0 max))))
                                     (pairs s s)))))]
           
           @predict[(print-stream (odd-sum-tripples 10))]}
