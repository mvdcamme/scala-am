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


@title{Examen Informatica tweede zit 1999: Streams vraag}
@section{Raam}
Schrijf een functie @scheme[(raam data n)] die als input een datastroom @scheme[data] neemt en hiervan een stroom van stromen maakt door elke @scheme[n] opeenvolgende data-elementen in een stroompje samen te nemen. Veronderstel dat de inputstroom oneindig is.

@image["streams/images/ex1999-1.png" #:scale 0.20]

@solution{@defs+int[((define (raam data n)
                       (define (split head-data data i)
                         (if (or (empty-stream? data) (>= i n))
                             (cons head-data data)
                             (split (append-streams head-data 
                                                    (cons-stream (head data) 
                                                                 the-empty-stream)) 
                                    (tail data ) 
                                    (+ i 1))))
                       
                       (if (empty-stream? data)
                           the-empty-stream
                           (let ((temp (split the-empty-stream data 0)))
                             (cons-stream (car temp) (raam (tail data) n))))))]}

@interaction[(print-m (raam (list->stream '(1 2 3 4 5 6 7  8 9 1 2 3 4 6 3 )) 2))]
@interaction[(print-m (raam (list->stream '(1 2 3 4 5 6 7  8 9 1 2 3 4 6 3 )) 5))]


@section{Test stroom}
Schrijf een functie @scheme[(test-n stream pred n)] die een stroom @scheme[stream] van stromen als input neemt en voor elk van deze stromen nagaat of @scheme[n] van de elementen van deze stroom voldoen aan de voorwaarde @scheme[pred]. Een stroom van booleans wordt teruggegeven. Wanneer de stroom minder dan @scheme[n] elementen bevat wordt @scheme[#f] teruggegeven.

@image["streams/images/ex1999-2.png" #:scale 0.20]

@solution{@defs+int[((define (stream-length s)
                       (if (empty-stream? s)
                           0
                           (+ 1 (stream-length (tail s)))))
                     
                     (define (test-n streams pred? n)
                       (map-stream (lambda (s)
                                     (>= (accumulate + 
                                                     0 
                                                     (map-stream (lambda (x) 
                                                                   (if (pred? x) 1 0)) 
                                                                 s))
                                         n))
                                   streams)))]}

@interaction[(print-stream (test-n (list->stream
                                    (list
                                     (list->stream '(1 1 1 1))
                                     (list->stream '(1 2 1 1))
                                     (list->stream '(3 5 7 9))))
                                   odd?
                                   4))]
@interaction[(print-stream (test-n (list->stream
                                    (list
                                     (list->stream '(1 1 1 1))
                                     (list->stream '(1 1 1 1 1))
                                     (list->stream '(3 5 7 9))))
                                   odd?
                                   4))]             
                                       
                                       @section{Extremen in neerslagmetingen}
                                       Volgens de definitie van het K.M.I. spreekt men van een droogte-periode als binnen een periode van 3 weken er 2- dagen minder dan 1 mm regen valt. Een periode wordt extreem nat genoemd als binnen een periode van 1 week er 4 dagen meer dan 10 mm regen valt. Schrijf m.b.v. streamoperatoren een functie @scheme[(extreme data)] die nagaat voor een gegeven stroom van neerslagmetingen of er doorgte of natte periodes in voorkomen. Teken een diagram voor je oplossing m.b.v. de gebruikte streamoperatoren.
                                       
                                       @solution{@defs+int[((define (extreme data)
                                                              (define (extreme-dry data)
                                                                (test-n (raam data  (* 3 7)) (lambda (x) (< x 1)) 20))
                                                              
                                                              (define (extreme-wet data)
                                                                (test-n (raam data  (* 1 7)) (lambda (x) (> x 10)) 4))
                                                              
                                                              (or (extreme-dry data)
                                                                  (extreme-wet data))))]}