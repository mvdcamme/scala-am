#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@hidden-code[(define (print-m matrix)
               (display "[")
               (print-stream (head matrix)) (newline)
               (stream-for-each (lambda (x) (display " ")(print-stream x) (newline)) (tail matrix))
               (display "]\n"))]


@title{Examen januari 2004: Streams vraag}
@section{Gradient}
Schrijf @scheme[gradient] die een stream van getallen als invoer neemt en als uitvoer een stream van getallen produceert. Deze geproduceerde stream van getallen bevar de absolute waarden van de verschillen van elke twee opeenvolgende getallen op de invoerstream (of gradiënt). Voorbeeld:

@image["streams/images/ex2004-1.png" #:scale 0.20]

@solution{@defs+int[((define (gradient s)
                       (map-stream (lambda (s)
                                     (if (or (empty-stream? s)
                                             (empty-stream? (tail s)))
                                         'X
                                         (let ((a (head s))
                                               (b (head (tail s))))
                                           (abs (- a b)))))
                                   
                                   (raam s 2))))]}

@interaction[(print-stream 
              (gradient (list->stream '(3 17 18 5 13 16 5 13 16 5 8 14 7))))]


@section{Alarm}
Schrijf @scheme[alarm] die een stream van getallen en een predikaat neemt als invoer. Hetgeen @scheme[alarm] doet, is een signaal geven als aan het predikaat voldaan is (door het woordje "alarm" en het overeenkomende getal af te drukken op het scherm). Voorbeeld:

@image["streams/images/ex2004-2.png" #:scale 0.20]

@solution{@defs+int[((define (alarm s p?)
                       (stream-for-each (lambda (e)
                                          (if (p? e) 
                                              (begin (display "alarm ")
                                                     (display e)
                                                     (newline))))
                                        s)))]}

@interaction[(alarm (list->stream '(3 17 18 5 13 16 5 8 14 7)) 
                    (lambda (n) (> n 10)))]

@section{Check}
In een constant draaiende fabriek is een stuk software vereist dat controleert of het productieproces onder controle is. Schrijf @scheme[check] die een stream van temperatuursmetingen en een drempelwaarde binnenkrijgt. @scheme[check] gaat nakijken of het verschil tussen twee opeenvolgende temperatuursmetingen beneden de gegeven drempelwaarde blijft en zal een alarm geven indien dit niet het geval is. Voorbeeld:

@image["streams/images/ex2004-3.png" #:scale 0.20]

@solution{@defs+int[((define (check s t)
                       (alarm (gradient s) 
                              (lambda (x) (and (number? x) (> x t))))))]}

@interaction[(check (list->stream '(129 93 111 87 85 103 100 134 121)) 20)]