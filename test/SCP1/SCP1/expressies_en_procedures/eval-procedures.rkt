#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Evaluatie van procedures (oefening 4.4 uit Simply Scheme)}
Elk van de volgende procedure definities bevat een bepaalde fout.
Geef aan wat fout is en corrigeer.
@itemize{@item{@scheme[(define (sphere-volume r)(* (/ 4 3) 3.14)(* r r r))]
               @solution[@def+int[(define (sphere-volume-correct r)
                                    (* (* r r r)
                                       (* (/ 4 3) 3.14)))]]}
          @item{@scheme[(define (next x) (x + 1))]
                @solution[@def+int[(define (next-correct x)
                                     (+ x 1))]]}
          
          
          @item{@scheme[(define (square) (* x x))]
                @solution[@def+int[(define (square x)
                                     (* x x))]]}
          
          
          @item{@scheme[(define (triangle-area triangle)(* 0.5 base height))]
                @solution[@def+int[(define (triangle-area-correct base height)
                                     (* 0.5 base height))]]}
          
          
          @item{@scheme[(define (sum-of-squares (square x) (square y))(+ (square x) (square y)))]
                @solution[@def+int[(define (sum-of-squares-correct x y)
                                     (+ (square x) (square y)))]]}}

