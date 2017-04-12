#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Omgevingsmodellen voor let* statement}

@section{Lambda}
Zet de @scheme[let*] om in de overeenkomstige lambda expressie.
Leg uit aan de hand van een omgevingsmodel-diagram waarom er deze keer niets misgaat.

@schemeblock[(let* ((x 1)
                    (y (+ 1 x)))
               (+ x y))]

@solution[@def+int[((lambda (x)
                      ((lambda (y)
                         (+ x y))(+ 1 x)))1)]]


@section{Omgevingsmodel-diagrammen voor @scheme{print-abc}}
Voorspel de output van @scheme[(foo 1 2 3)] aan de hand van omgevingsmodel-diagrammen.

@defs+int[((define (print-abc a b c)
             (display a) (display " ")
             (display b) (display " ")
             (display c) (newline))
           
           (define (foo a b c)
             (print-abc a b c)
             (let* ((a 4)
                    (c 5)
                    (b c))
               (print-abc a b c)
               (let* ((b 6)
                      (c a))
                 (print-abc a b c))
               (let* ((a b)
                      (c a))
                 (print-abc a b c)))
             (print-abc a b c)))]

@solution[@interaction[(foo 1 2 3)]]

@solution[ @itemize[#:style 'ordered]{
     @item{ @image["hogere_orde/images/let*/let*-box1.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box2.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box3.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box7.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box8.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box11.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box12.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box17.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box18.png"] }
     @item{ @image["hogere_orde/images/let*/let*-box25.png"] }}]


@section{Recursieve procedure in een let}
Kan je onderstaande uitdrukking doen werken door @scheme[let*] te gebruiken? Waarom (niet)? 
@schemeblock[(let ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))))
               (fac 3))]

@solution[@interaction[(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))))
                         (fac 3))]]
