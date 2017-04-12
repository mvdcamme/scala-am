#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math

@title{ADT Lijnstuk & Punt}
@section{Punt}
Definieer een data-structuur om een (2-dimensionaal) punt voor te stellen.
Schrijf hiertoe een constructor @scheme[(make-punt x y)] en
de selectoren @scheme[(x punt)] en @scheme[(y punt)].

@solution{Simple:
          @defs+int[((define (make-punt x y) (cons x y))
                     (define (x punt)        (car punt))
                     (define (y punt)        (cdr punt)))]}

@interaction[(define a (make-punt 2 11))
             a
             (x a)
             (y a)]


@solution{Tagged:
          @defs+int[((define (make-punt-tag x y) (list 'punt x y))
                     (define (punt? punt)
                       (and (pair? punt)  (eq? (car punt) 'punt)))
                     (define (x-tag punt)            (cadr  punt))
                     (define (y-tag punt)            (caddr punt))
                     (define a-tag (make-punt-tag 2 11)))
                     a-tag
                     (x-tag a-tag)
                     (y-tag a-tag)]
          
          Macho style:
          @defs+int[((define make-punt-macho cons)
                     (define x-macho car)
                     (define y-macho cdr)
                     (define a-macho (make-punt-macho 2 11)))
                     a-macho
                     (x-macho a-macho)
                     (y-macho a-macho)]}

@section{Lijnstuk}
Definieer een data-structuur om een lijnstuk voor te stellen d.m.v. 
het start- en eindpunt van het lijnstuk.
Schrijf hiertoe een constructor @scheme[(make-segment start einde)] en
de selectoren @scheme[(start-punt segment)] en @scheme[(end-punt segment)].

@solution{@defs+int[((define (make-segment start end) 
                       (cons start end))
                     (define (start segment) (car segment))
                     (define (end segment) (cdr segment)))]
           
           @defs+int[((define (make-segment-tag x y)   
                        (list 'segment x y))
                      (define (segment? segment)
                        (and (pair? segment) 
                             (eq? (car segment) 'segment)))
                      (define (start-tag segment) (cadr  segment))
                      (define (end-tag segment) (caddr segment)))]
           
           @defs+int[((define make-segment-macho cons)
                      (define start-macho car)
                      (define end-macho cdr))]}






@section{middelpunt}
Gebruik de operaties van dit ADT om een procedure @scheme[(middelpunt segment)] te schrijven
die het middelpunt van een lijnstuk teruggeeft.
Hint: @math-in{M=\left(\frac{x_1+x_2}{2},\frac{y_1+y_2}{2}\right)}

@solution[@defs+int[((define (gemiddelde a b) (/ (+ a b) 2))
                     
                     (define (middelpunt segment)
                       (let ((start (start segment))
                             (end (end segment)))
                         (make-punt (gemiddelde (x start) (x end))
                                    (gemiddelde (y start) (y end)))))
                     
                     (define (middelpunt-tag segment)
                       (let ((start (start-tag segment))
                             (end (end-tag   segment)))
                         (make-punt-tag 
                          (gemiddelde (x-tag start) (x-tag end))
                          (gemiddelde (y-tag start) (y-tag end))))))]
           
           @def+int[(define (middelpunt-macho segment)
                      (let ((start (start-macho segment))
                            (end (end-macho segment)))
                        (make-punt-macho 
                         (gemiddelde (x-macho start) (x-macho end))
                         (gemiddelde (y-macho start) (y-macho end)))))]]

