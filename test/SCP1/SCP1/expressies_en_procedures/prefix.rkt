#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math @; put this ine ach file that uses formula's!

@title{Prefixnotatie}

@section[#:tag "infix->prefix"]{Van infix- naar prefixnotatie}
Schrijf op een blad papier de volgende expressies in Scheme-notatie (prefixnotatie).
Indenteer om de leesbaarheid te verhogen.

@itemize{@item{@math-in{\frac{a + b}{e} - \frac{c + d}{f}}
                @solution[@schemeblock[(- (/ (+ a b)
                                             e)
                                          (/ (+ c d)
                                             f))]]}
          
          
          @item{@math-in{c + \frac{a}{b \times c + \frac{d}{e + \frac{f}{g}}}}
                 
                 @solution[@schemeblock[(+ c
                                           (/ a
                                              (+ (* b c)
                                                 (/ d 
                                                    (+ e 
                                                       (/ f g))))))]]}
          
          
          @item{@math-in{\frac{a+\frac{b}{c}}{d} \times \frac{e}{\frac{g}{i}-h}}
                 @solution[@schemeblock[(* (/ (+ a
                                                 (/ b c))
                                              d)
                                           (/ e
                                              (- (/ g i)
                                                 h)))]]}}


@section{Verifieer syntax}
Verifieer je antwoorden uit deel @secref{infix->prefix} door ze uit te proberen in een Scheme vertolker.

@section[#:tag "zero"]{Maak Nul}
Bedenk waarden voor de parameters a tot en met h zodat de eerste twee expressies 0 en de laatste 1 voorstellen.


@section{Verifieer waarden}
Verifieer opnieuw de correctheid van je antwoord uit deel @secref{zero} door het uit te proberen in Scheme.
@solution[@interaction[(let ((a  1) (b  1) (e  2)
                                    (c  2) (d -1) (f  1))
                         (- (/ (+ a b) e) (/ (+ c d) f)))
                       
                       (let ((a -6) (b  1) (e  2)
                                    (c  2) (d  3) (f  1)
                                    (g  1))
                         (+ c (/ a (+ (* b c) (/ d (+ e (/ f g)))))))
                       
                       
                       (let ((a 0) (b 1) (c 1)
                                   (d 1) (e 1) (f 1)
                                   (g 2) (h 1) (i 1))
                         (* (/ (+ a (/ b c)) d) (/ e (- (/ g i) h))))]]

