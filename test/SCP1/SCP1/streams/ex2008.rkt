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


@title{Examen januari 2008: Streams vraag}
@section{Prune}
Schrijf een procedure @scheme[prune] die een eventueel oneindige stroom van elementen een een getal @scheme[n] als invoer neemt en als uitvoer een stroom produceert waarin afwisselend @scheme[n] elementen van de inputstroom doorgelaten of weggelaten worden. Op de resultaatstroom staan dus getallen 1 tot n, 2n+1 tot 3n, 4n+1 tot 5n, enz. uit de oorspronkelijke stroom.

@image["streams/images/ex2008-1.png" #:scale 0.20]

@solution{@defs+int[((define (prune s n)
                       (define (aux s d i)
                         (cond
                           ((empty-stream? s) the-empty-stream)
                           ((>= i n) (aux s (not d) 0))
                           (d (cons-stream (head s) (aux (tail s) d (+ i 1))))
                           (else (aux (tail s) d (+ i 1)))))
                       (aux s #t 0)))]}

@interaction[(print-stream (prune (list->stream '(1 2 3 4 5 6 7 8 9)) 2))]


@section{Split}
Schrijf een procedure @scheme[split] die een eventueel oneindige stroom van elementen en een getal @scheme[n] als invoer neemt en als uitvoer een stroom produceert van stromen van @scheme[n] lang. Op de resultaatstroom staan dus kleine eindige stroompjes van @scheme[n] elementen.

@image["streams/images/ex2008-2.png" #:scale 0.20]

@solution{@defs+int[((define (split s n)
                       (define (aux t s i)
                         (cond
                           ((empty-stream? s) (cons t the-empty-stream))
                           ((>= i n) (cons t s))
                           (else (aux (append-streams t 
                                                      (list->stream (list(head s)))) 
                                      (tail s) 
                                      (+ i 1)))))
                       
                       (if (empty-stream? s)
                           the-empty-stream
                           (let ((splitted (aux the-empty-stream s 0)))
                             (cons-stream (car splitted) (split (cdr splitted) n))))))]}

@interaction[(print-m (split (list->stream '(1 2 3 4 5 6 7 8 9)) 2))]


@section{Daggemiddelden}
Het weerstation in Ukkel zendt om het uur een temperatuur door (dag in dag uit) op een stroom. Op de temperaturenstroom staan dus afwisselend 12 dagtemperaturen en 12 nachttemperaturen. De stroom begint met 12 dagtemperaturen. Construeer een stroom (@scheme[daggemiddelden]) van gemiddelden tijdens de dag. Teken eerst een gedetailleerd schema van je oplossing.

@solution{@defs+int[((define (daggemiddelden s)
                       (map-stream 
                        (lambda (s) 
                          (let ((x (accumulate (lambda (a b) 
                                                 (cons (+ a (car b)) (+ 1 (cdr b))))
                                               (cons 0 0) 
                                               s)))
                            (/ (car x) (cdr x))))
                        (prune (split s 12) 1))))]}



@section{Nachtgemiddelden}
Doe ditzelfde voor nachtgemiddelden.

@solution{@defs+int[((define (nachtgemiddelden s)
                       (map-stream 
                        (lambda (s) 
                          (let ((x (accumulate (lambda (a b) 
                                                 (cons (+ a (car b)) (+ 1 (cdr b))))   
                                               (cons 0 0) 
                                               s)))
                            (/ (car x) (cdr x))))
                        (prune (tail (split s 12)) 1))))]}