#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@title{Som kwadraten oneven elementen}
Schrijf een procedure @scheme[sum-odd-squares] die de som van de kwadraten van de oneven elementen uit een stream berekent door gebruik te maken van @scheme[map-stream], @scheme[streamfilter] en @scheme[accumulate].

@solution{@defs+int[((define (square x)(* x x))
                     
                     (define (sum-odd-squares stream)
                       (accumulate + 0 (map-stream square (streamfilter odd? stream)))))]
           
           @predict[(sum-odd-squares (list->stream '(1 2 3 4 5 6)))]}
