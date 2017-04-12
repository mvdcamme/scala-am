#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")
@hidden-code[(define (atom? x) (not (pair? x)))]

@title{Oefening 2.53 uit Abelson&Sussman}


Wat is het resultaat van volgende expressies:
@itemize{@item[@predict[(list 'a 'b 'c)]]
          @item[@predict[(list (list 'george))]]
          @item[@predict[(cdr '((x1 x2) (y1 y2)))]]
          @item[@predict[(cadr '((x1 x2) (y1 y2)))]]
          @item[@predict[(atom? (car '(a short list)))]]
          @item[@predict[(memq 'red '((red shoes) (blue socks)))]]
          @item[@predict[(memq 'red '(red shoes blue socks))]]}