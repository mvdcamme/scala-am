#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 3.16 uit Abelson&Sussman}
Je wil een functie schrijven die het aantal cons-cellen in een structuur telt. Toon aan dat onderstaande procedure @scheme[count-pairs] niet het gewenste resultaat geeft door box-pointer diagrammen met 3 cellen te tekenen, waarvoor het resultaat van @scheme[count-pairs] respectievelijk resulteert in 3, 4, 7 en een oneindige lus. Schrijf expressies om die structuren te maken.

@def+int[(define (count-pairs x)
           (if (not (pair? x))
               0
               (+ (count-pairs (car x))
                  (count-pairs (cdr x))
                  1)))]

@itemize[#:style 'ordered]{@item{@solution[@def+int[(define ret3 (cons 'a (cons 'b (cons 'c '()))))]]
                                   @solution{@image["destructieve/images/bp-ret3.png" #:scale 0.14]}
                                  @interaction[(count-pairs ret3)]}
                            @item{@solution[@def+int[(define ret4
                                                       (let ((last (cons 'c '())))
                                                         (cons last (cons 'b last))))]]
                                   @solution{@image["destructieve/images/bp-ret4.png" #:scale 0.14]}
                                   @interaction[(count-pairs ret4)]}
                            @item{@solution[@def+int[(define ret7
                                                       (let* ((last (cons 'c '()))
                                                              (middle (cons last last)))
                                                         (cons middle middle)))]]
                                   @solution{@image["destructieve/images/bp-ret7.png" #:scale 0.14]}
                                   @interaction[(count-pairs ret7)]}
                            @item{@solution[@def+int[(define retno
                                                       (let* ((last (cons 'c '()))
                                                              (lst (cons 'a (cons 'b last))))
                                                         (set-cdr! last lst)
                                                         lst))]]
                                   @solution{@image["destructieve/images/bp-retno.png" #:scale 0.14]}}}

