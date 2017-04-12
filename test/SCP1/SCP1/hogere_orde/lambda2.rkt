#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Herhaling lambda-notatie}
Geef voor elk van de volgende expressies een definitie van @scheme[f] 
zodanig dat de expressie de waarde 5 teruggeeft.

@itemize[#:style 'ordered]{ @item{@solution[@def+int[(define f 5)]]
                                   @interaction[f]}
                            @item{@solution[@def+int[(define f (lambda () 5))]]
                                   @interaction[(f)]}
                            @item{@solution[@def+int[(define f (lambda (x) 5))]]
                                   @interaction[(f 3)]}
                            @item{@solution[@def+int[(define f (lambda () (lambda () 5)))]]
                                   @interaction[((f))]}
                            @item{@solution[@def+int[(define f (lambda () (lambda () (lambda (drie) 5))))]]
                                   @interaction[(((f)) 3)]}}

