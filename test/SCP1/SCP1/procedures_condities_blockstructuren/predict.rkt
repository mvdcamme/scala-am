#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Evaluatie van procedures}
Voorspel het resultaat van onderstaande expressies
(veronderstel dat ze in volgorde geëvalueerd worden):
@itemize[@item[@predict[(+ 5 4 3)]]
          @item[@predict[(define a 3)]]
          @item[@predict[(define b (+ a 1))]]
          @item[@predict[(+ a b (* a b))]]
          @item[@predict[(= a b)]]
          @item[@predict[(if (> a b) a b)]]
          @item[@predict[(if (and (> b a) (< b (* a b))) b a)]]
          @item[@predict[(+ 2 (if (> a b) a b))]]
          @item[@predict[(* (cond ((> a b) a)((< a b) b)(else -1))(+ a 1))]]
          @item[@predict[((if (< a b) + -) a b)]]]
