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


@title{Examen januari 2010}
De NMBS wil de tevredenheid van zijn treinreizigers in kaart brengen. Hiervoor hebben ze een systeem ontwikkeld waarbij treinreizigers dagelijks hun tevredenheid (een cijfer tussen 0 en 10) kunnen invullen. Deze dagelijkse tevredenheidscijfers kunnen worden voorgesteld als een oneindige stream van getallen met een # als scheidingsteken tussen de dagen (zie figuur voor een voorbeeldstream). Van deze tevredenheidsgetallen wil de NMBS per dag het minimum, maximum en gemiddelde weten.

@section{Dagcijfers}
Schrijf een procedure @scheme[geef-dagcijfers] die gegeven een oneindige stream van tevredenheidscijfers een oneindige stream van streams teruggeeft door de opeenvolgende dagcijfers same te nemen (de dagen worden gescheiden door een #).

@image["streams/images/ex2010-1.png" #:scale 0.20]

@solution{@defs+int[((define (split-at-hash stream)
                       (define (pre-hash stream) 
                         (if (or (empty-stream? stream) (eq? "#" (head stream)))
                             the-empty-stream
                             (cons-stream (head stream) (pre-hash (tail stream)))))
                       
                       (define (post-hash stream) 
                         (cond ((empty-stream? stream) the-empty-stream)
                               ((eq? "#" (head stream)) (tail stream))
                               (else (post-hash (tail stream)))))
                       
                       (if (empty-stream? stream)
                           the-empty-stream
                           (cons-stream (pre-hash stream)
                                        (split-at-hash (post-hash stream)))))
                     
                     (define (geef-dagcijfers stream)
                       (split-at-hash stream)))]}

@interaction[(print-m (geef-dagcijfers (list->stream '(1 2 3 4 5 "#" 2 3 4 5 "#" 5 6 7))))]


@section{Bereken resultaten}
Schrijf voor de NMBS een procedure @scheme[bereken-resultaten] die gegeven een oneindige stream van tevredenheidscijfers een oneindige stream van streams teruggeeft. Deze resultaat-stream zal per dag een stream bevatten met 3 elementen (het minimum, maximum en gemiddelde).

Deze operatoren staan alvast tot je beschikking: @scheme[streamfilter], @scheme[map-stream], @scheme[accumulate], @scheme[append-stream], @scheme[flatten], @scheme[accumulate-n], @scheme[cons-stream].

@image["streams/images/ex2010-2.png" #:scale 0.20]

@solution{@defs+int[((define (bereken-resultaten stream)
                       (map-stream 
                        (lambda (dag-stream)
                          (cons-stream (accumulate min 10 dag-stream)
                                       (cons-stream
                                        (accumulate max 0 dag-stream)
                                        (cons-stream
                                         (/ (accumulate + 0 dag-stream)
                                            (accumulate + 
                                                        0 
                                                        (map-stream 
                                                         (lambda (x) 1) 
                                                         dag-stream)))
                                         the-empty-stream))))
                        (geef-dagcijfers  stream))))]}


@interaction[(print-m (bereken-resultaten (list->stream '(1 2 3 4 5 "#" 2 3 4 5 "#" 5 6 7))))]

