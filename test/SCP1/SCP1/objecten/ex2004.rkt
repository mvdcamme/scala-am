#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen januari 2004: Object vraag}
@section{Buffer}
Een buffer is een datastructuur die vaak gebruikt wordt voor tijdelijke of tussentijdse opslag van gegevens. In Scheme kunnen we een buffer implementeren als een object. Beschouw het @scheme[buffer] object dat bedoeld is om alleen maar getallen op te slaan en volgende boodschappen verstaat:
@itemize[#:style 'ordered]{
                           @item{@bold{@scheme[newValue]}: voegt een nieuwe waarde toe aan de buffer.}
                            @item{@bold{@scheme[return]}: geeft de inhoud van de buffer terug.}
                            @item{@bold{@scheme[returnSum]}: geeft de som van alle getallen die in de buffer zijn opgeslagen terug.}
                            @item{@bold{@scheme[flush]}: maakt de buffer leeg.}}

Implementeer het object @scheme[buffer] dat dit gedrag kan voorstellen.

@solution{@defs+int[((define (maak-buffer)
                       (let ((inhoud '()))
                         
                         (define (newValue value)
                           (set! inhoud (append inhoud (list value))))
                         
                         (define (returnSum)
                           (apply + inhoud))
                         
                         (define (flush)
                           (set! inhoud '()))
                         
                         (define (value pos)
                           (list-ref inhoud pos))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'newValue) newValue)
                                 ((eq? msg 'return) inhoud)
                                 ((eq? msg 'returnSum) (returnSum))
                                 ((eq? msg 'flush) (flush))
                                 ((eq? msg 'value) value)
                                 ((eq? msg 'size) (length inhoud))
                                 (else (error "wrong message"))))
                         dispatch)))]
           
           @interaction[(define buffer (maak-buffer))]
           @interaction[((buffer 'newValue) 3)]
           @interaction[((buffer 'newValue) 9)]
           @interaction[(buffer 'returnSum)]      
           @interaction[(buffer 'return)]         
           @interaction[(buffer 'flush)]
           @interaction[(buffer 'return)]}

@section{Verkeersteller}
De Brusselse overheid wilt een beter overzicht krijgen van de verkeerssituatie in de hoofdstad. Daarom besluit ze om de belangrijkste kruispunten uit te rusten met een verkeersteller. De software voor zo'n verkeersteller wordt in Scheme geïmplementeerd en voorgesteld als een object dat de volgende boodschappen verstaat:
@itemize[#:style 'ordered]{
                           @item{@bold{@scheme[newCar]}: voor elke auto die langs de verkeersteller rijdt wordt deze boodschap gestuurd, er wordt een tellertje opgehoogd dat het aantal voorbijgereden auto's bijhoudt.}
                            @item{@bold{@scheme[newHour]}: op elk volledig uur wordt dit bericht naar de verkeersteller gestuurd, de teller wordt uitgeschreven naar een buffer en de teller wordt weer op nul gezet.}
                            @item{@bold{@scheme[newDay]}: op het einde van de dag (na 24 uur) wordt dit bericht naar de verkeersteller gestuurd, er wordt een overzicht gegeven van de dag (aantal auto's die voorbijgereden zijn + het drukste uur, d.w.z. het uur zelf en het aantal auto's dat er in dat uur voorbijgereden zijn). Tenslotte wordt de buffer leeggemaakt en de teller op nul gezet voor de volgende dag.}}

Implementeer het object @scheme[verkeersteller] dat dit gedrag kan voorstellen. Maak hierbij gebruik van een @scheme[counter] object (dat de boodschappen @scheme[increment], @scheme[read] en @scheme[reset] verstaat) en het @scheme[buffer] object.

@solution{@defs+int[((define (make-counter)
                       (let ((state 0))    
                         (define (increment) (set! state (+ state 1)))
                         (define (read) state)
                         (define (reset) (set! state 0))
                         (define (dispatch msg)
                           (cond ((eq? msg 'increment) (increment))
                                 ((eq? msg 'read) (read))
                                 ((eq? msg 'reset) (reset))
                                 (else (error "wrong message"))))
                         dispatch))
                     
                     (define (maak-verkeersteller)
                       (let ((voorbijgereden (make-counter))
                             (buffer (maak-buffer)))
                         
                         (define (newCar)
                           (voorbijgereden 'increment))
                         
                         (define (newHour)
                           ((buffer 'newValue) (voorbijgereden 'read))
                           (voorbijgereden 'reset))
                         
                         (define (newDay)
                           (define (loop start end)
                             (cond ((= start end) (newline))
                                   (else (display "Tussen ") (display start) 
                                         (display " en ") (display (+ start 1))
                                         (display " uur : ")
                                         (display ((buffer 'value) start)) 
                                         (display " auto's")
                                         (newline)
                                         (loop (+ start 1) end))))
                           (if (= (buffer 'size) 24)
                               (begin (loop 0 24)
                                      (buffer 'flush)
                                      (voorbijgereden 'reset))
                               (error "no 24 hours have passed")))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'newCar) (newCar))
                                 ((eq? msg 'newHour) (newHour))
                                 ((eq? msg 'newDay) (newDay))
                                 (else (error "wrong message"))))
                         dispatch)))]
           
           @interaction[(define verkeersteller (maak-verkeersteller))]
           @interaction[(verkeersteller 'newCar)]     
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]                   
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newHour)]                  
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]    
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newHour)]                   
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)] 
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]    
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]     
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newCar)]     
           @interaction[(verkeersteller 'newHour)]     
           @interaction[(verkeersteller 'newCar)]
           @interaction[(verkeersteller 'newDay)]
           @interaction[(verkeersteller 'newHour)]
           @interaction[(verkeersteller 'newDay)]}