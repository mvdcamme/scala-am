#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@hidden-code[(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))]


@title{Triplets}
Schrijf een procedure die een (oneindige) stream genereert van triplets (lijsten met drie getallen) i, j, k die voldoen aan de eigenschap dat i + j > k (m.a.w. waarvan de som van de eerste twee elementen strikt groter is dan het derde element).

@bold{Hint}: Maak gebruik van het feit dat k in het interval [1, i+j] ligt!

@solution{@defs+int[((define empty-list-stream (cons-stream '() empty-list-stream))
                     
                     (define (my-filter l)
                       (let ((i (car l))
                             (j (cadr l))
                             (k (caddr l)))
                         (> (+ i j) k)))
                     
                     ;(print-inf-stream (streamfilter my-filter (pairs (tail ints) (pairs (tail ints) (pairs (tail ints) empty-list-stream)))) 10)
                     
                     (define (triplets)
                       (map-stream (lambda (x)
                                     (let ((i (car x))(j (cdr x)))
                                       (map-stream (lambda (k)
                                                     (list i j k))
                                                   (enumerate-interval 1 (+ i j)))))
                                   (pairs (tail integers) (tail integers)))))]}

@interaction[(print-inf-stream (triplets) 4)]

