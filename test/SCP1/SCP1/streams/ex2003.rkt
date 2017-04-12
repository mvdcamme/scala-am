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


@title{Examen januari 2003: Streams vraag}
@section{Cut delimiter}
Schrijf een procedure @scheme[cut-delimiter] die één (eventueel oneindige) stroom van elementen en een delimiter als invoer neemt en als uitvoer een stroom van kleine stromen produceert. De invoerstroom zal dus door deze procedure opgesplitst worden volgens de delimiter.

Voorbeeld:

@image["streams/images/ex2003-1.png" #:scale 0.20]

@solution{@defs+int[((define (split stream pred?)
                       (define (aux temp stream)
                         (cond
                           ((empty-stream? stream) 
                            (list temp 'x the-empty-stream ))
                           ((pred? (head stream))  
                            (list temp (head stream) (tail stream)))
                           (else (aux (append-streams temp 
                                                      (cons-stream (head stream) 
                                                                   the-empty-stream)) 
                                      (tail stream)))))
                       (aux the-empty-stream stream))
                     
                     (define (cut-delimiter stream delimiter)
                       (let* ((pred? (lambda (x) (char=? x delimiter)))
                              (splited (split stream pred?))
                              (first (car splited))
                              (rest (caddr splited)))
                         (if (empty-stream? rest)
                             (cons-stream first the-empty-stream)
                             (cons-stream first (cut-delimiter rest delimiter))))))]}

@interaction[(define space (car(string->list " ")))]
@interaction[(print-m 
              (cut-delimiter 
               (list->stream (string->list "hello world it is")) 
               space))]

@section{Filter tekstbestand}
We kunnen nu deze procedure gaan gebruiken om vieze woorden uit een tekst te halen. Veronderstel dat we een tekstbestand hebben. Dit tekstbestand wordt in dit geval voorgesteld als een stroon van karakters. Verder hebben we ook nog een eindig bestand met vieze woorden die in deze tekst kunnen voorkomen. Schrijf nu een procedure die aan de hand van @scheme[cut-delimiter] en de gekende operaties op stromen alle vieze woorden die in de tekst voorkomen uit de tekst verwijdert en vervangt door een eindige stroom die het woordje biep" bevat in de plaats van dit vies woord. Het eindresultaat is dus een stroom van stromen die de gekuiste tekst voorstelt.

@bold{Hint}: schrijf (en gebruik) een procedure die de inhoud van twee stromen met elkaar vergelijkt.

Voorbeeld:

@image["streams/images/ex2003-2.png" #:scale 0.20]

@solution{@defs+int[((define (kuis-tekst stream-words stream-not-ok)
                       (define (censor eq? stream-not-ok a-stream)
                         (cond
                           ((empty-stream? stream-not-ok) a-stream)
                           ((eq-stream? eq? (head stream-not-ok) a-stream)
                            (list->stream (string->list "biep")))
                           (else (censor eq? (tail stream-not-ok) a-stream))))
                       
                       (define (eq-stream? eq? s1 s2)
                         (cond
                           ((empty-stream? s1)(empty-stream? s2))
                           ((empty-stream? s2)#f)
                           (else (and (eq? (head s1) (head s2))
                                      (eq-stream? eq? (tail s1) (tail s2))))))
                           
                       (map-stream (lambda (s) (censor char=? stream-not-ok s)) 
                                   (cut-delimiter stream-words space))))]}

@interaction[(print-m 
              (kuis-tekst 
               (list->stream (string->list "hello world sex and drugs"))
               (cut-delimiter (list->stream (string->list "sex drugs")) space)))]