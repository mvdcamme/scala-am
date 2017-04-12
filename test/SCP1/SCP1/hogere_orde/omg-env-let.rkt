#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Omgevingsmodellen voor let statement}
Leg uit wat er mis gaat bij evaluatie van de volgende expressie.
Hint: Zet de let om in de overeenkomstige lambda expressie en
teken dan het omgevingsmodel diagram.

;! code
'(let ((x 1)
               (y (+ 1 x)))
           (+ x y))]]

'((lambda (x y) (+ x y)) 1 (+ 1 x))
;! omgevingsmodel
;! solution


;??
Voorspel de output van @scheme[(foo 1 2 3)} aan de hand van omgevingsmodel-diagrammen.

;! code
(define (print-abc a b c)
  (display a) (display " ")
  (display b) (display " ")
  (display c) (newline))

(define (foo a b c)
  (print-abc a b c)
  (let ((a 4)
        (c 5)
        (b c))
    (print-abc a b c)
    (let ((b 6)
          (c a))
      (print-abc a b c))
    (let ((a b)
          (c a))
      (print-abc a b c)))
  (print-abc a b c))
;! code

;! solution
(examples print-abc
          (foo 1 2 3)))
;! solution

@solution[ @itemize[#:style 'ordered]{
     @item{ @image["hogere_orde/images/let/let-box1.png"] }
     @item{ @image["hogere_orde/images/let/let-box2.png"] }
     @item{ @image["hogere_orde/images/let/let-box3.png"] }
     @item{ @image["hogere_orde/images/let/let-box5.png"] }
     @item{ @image["hogere_orde/images/let/let-box6.png"] }
     @item{ @image["hogere_orde/images/let/let-box8.png"] }
     @item{ @image["hogere_orde/images/let/let-box9.png"] }
     @item{ @image["hogere_orde/images/let/let-box12.png"] }
     @item{ @image["hogere_orde/images/let/let-box13.png"] }
     @item{ @image["hogere_orde/images/let/let-box17.png"] }}]
