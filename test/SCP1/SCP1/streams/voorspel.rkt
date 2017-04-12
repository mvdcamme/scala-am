#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@title{Delayed evaluation}
Indien streams geïmplementeerd worden met "delayed evaluation" (d.m.v. @scheme[delay] en @scheme[force]), voorspel dan wat er op het scherm verschijnt als de volgende expressies in de opgegeven volgorde geëvalueerd worden.

@defs+int[((define (fac n)
             (display "->")(display n)(newline)
             (if (= n 0)
                 1
                 (* n (fac (- n 1))))))]

@itemize[#:style 'ordered]{
                           @item{@predict[(define ds (cons-stream (fac 3) (cons-stream (fac 2) (cons-stream (fac 1) the-empty-stream))))]}
                            @item{@predict[(head ds)]}
                            @item{@predict[(head (tail ds))]}
                            @item{@predict[(tail ds)]}
                            @item{@predict[(head (tail (tail ds)))]}}