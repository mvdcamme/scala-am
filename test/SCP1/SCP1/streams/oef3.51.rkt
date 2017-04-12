#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@title{Oefening 3.51 uit Abelson&Sussman}
Indien streams geïmplementeerd worden met "delayed evaluation", voorspel en verklaar het resultaat van de volgende expressies, waarbij @scheme[show] als volgt gedefinieerd is:

@defs+int[((define (show x)
             (display x)(newline)
             x))]

@itemize[#:style 'ordered]{
                           @item{@predict[(define x (map-stream show (enumerate-interval 0 10)))]}
                            @item{@predict[(head (tail x))]}
                            @item{@predict[(head (tail x))]}
                            @item{@predict[(define x (map-stream show (enumerate-interval 0 10)))]}
                            @item{@predict[(head (tail (tail (tail x))))]}}