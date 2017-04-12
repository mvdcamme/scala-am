#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@title{Integers}
Vervolledig de volgende definitie van de @scheme[integers] stream:
@scheme[(define integers (cons-stream 1 (map-stream ??? integers)))]

@solution{@defs+int[((define (incr n) (+ n 1))
                     
                     (define integers (cons-stream 1 
                                                 (map-stream incr integers))))]}

@interaction[(print-inf-stream integers 5)]

