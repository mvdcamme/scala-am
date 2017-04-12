#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@hidden-code[(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))]


@title{Streamfilter}
Gebruik @scheme[streamfilter] om een stream van alle integers die niet deelbaar zijn door 2, 3 of 5 te bekomen.

@solution{@defs+int[((define (div? deeltal deler)
                       (= 0 (modulo deeltal deler)))
                     
                     (define (integers-special stream)
                       (streamfilter (lambda (x) (not (or (div? x 2)
                                                          (div? x 3)
                                                          (div? x 5)))) 
                                     stream))
                     
                     (define (make-not-div?-pred div)
                       (lambda (x) (not (div? x div))))
                     
                     (define (integers-special2 stream)
                       (streamfilter (make-not-div?-pred 2)
                                     (streamfilter (make-not-div?-pred 3)
                                                    (streamfilter (make-not-div?-pred 5) 
                                                                  stream)))))]}

@interaction[(print-inf-stream (integers-special integers) 5)]
