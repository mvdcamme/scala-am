#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Box-and-pointer representatie}

Geef de box-and-pointer representatie en
de print representatie van volgende expressies:
@schemeblock[(list 1 2)
             (cons 1 (list 2))
             (cons 1 (cons 2 '()))
             (list (list 1 2) (list 3 4))
             (list 1 2 (cons 3 (cons 4 5)))]

@solution[ @itemize[#:style 'ordered]{
     @item{ @image["lijsten/images/box-pointer1/bp1.png" #:scale 0.1] }
     @item{ @image["lijsten/images/box-pointer1/bp1.png" #:scale 0.1] }
     @item{ @image["lijsten/images/box-pointer1/bp1.png" #:scale 0.1] }
     @item{ @image["lijsten/images/box-pointer1/bp2.png" #:scale 0.1] }
     @item{ @image["lijsten/images/box-pointer1/bp3.png" #:scale 0.1] }}]