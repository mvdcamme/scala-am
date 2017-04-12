#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Omgevingsmodellen}
Teken de omgevingsmodel-diagrammen voor de evaluatie van enerzijds 
@scheme[(square 5)] en anderzijds @scheme[(f 5)] in de veronderstelling
dat de volgende procedure-definities gegeven zijn:

@defs+int[((define (square x)
             (* x x))
           
           (define (sum-of-squares x y)
             (+ (square x) (square y)))
           
           (define (f a)
             (sum-of-squares (+ a 1) (* a 2))))]

@solution[@interaction[(square 5)(f 5)]]
                                      
@solution[ @itemize[#:style 'ordered]{@item{ @image["expressies_en_procedures/images/sum-of-squares/sqs-box2.png"] }
                                       @item{ @image["expressies_en_procedures/images/sum-of-squares/sqs-box3.png"] }
                                       @item{ @image["expressies_en_procedures/images/sum-of-squares/sqs-box6.png"] }
                                       @item{ @image["expressies_en_procedures/images/sum-of-squares/sqs-box7.png"] }
                                       @item{ @image["expressies_en_procedures/images/sum-of-squares/sqs-box8.png"] }
                                       @item{ @image["expressies_en_procedures/images/sum-of-squares/sqs-box10.png"] }}] 