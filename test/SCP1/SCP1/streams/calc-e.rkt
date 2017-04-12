#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@title{Stream met benaderingen van @scheme[e]}
Ontwerp een oneindige stream van reële getallen die steeds een betere benadering van @scheme[e] teruggeeft:

@solution{@defs+int[((define (fac x) 
                       (if (= x 0) 1 (* x (fac (- x 1)))))
                     
                     (define (sub-e-stream index vorige-element)
                         (cons-stream vorige-element
                                      (sub-e-stream 
                                       (+ 1 index) 
                                       (+ vorige-element (/ 1.0 (fac index))))))
                     
                     (define e-stream (sub-e-stream 0 0)))]}

@interaction[(print-inf-stream e-stream 5)]