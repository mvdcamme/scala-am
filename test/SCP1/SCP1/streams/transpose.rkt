#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@hidden-code[(define integers (cons-stream 1 (map-stream (lambda (x) (+ x 1)) integers)))]


@title{Transpose}
Veronderstel dat elke matrix wordt voorgesteld als een stream van streams (elke stream stelt een rij van de matrix voor). Implementeer dan de volgende operatie @scheme[(transpose m)] die een matrix n teruggeeft waarbij: n[i,j] = m[j,i].

Hiervoor kan je gebruik maken van @scheme[accumulate-n]:

@defs+int[((define (accumulate-n op ne streams)
             (if (empty-stream? (head streams)) the-empty-stream
                 (let ((heads (map-stream head streams))
                       (tails (map-stream tail streams)))
                   (cons-stream (accumulate op ne heads)
                                (accumulate-n op ne tails))))))]

@solution{@defs+int[((define (transpose matrix)
                       (accumulate-n (lambda (x y)
                                       (cons-stream x y)) the-empty-stream matrix)))]}

@defs+int[((define (print-m matrix)
             (display "[")
             (print-stream (head matrix)) (newline)
             (stream-for-each (lambda (x) (display " ")
                                (print-stream x) (newline)) 
                              (tail matrix))
             (display "]\n"))
           
           (define matrix 
             (cons-stream (enumerate-interval 1 3)
                          (cons-stream (enumerate-interval 4 6)
                                       (cons-stream (enumerate-interval 7 9)
                                                    (cons-stream (enumerate-interval 10 12)
                                                                 the-empty-stream))))))]

@interaction[(print-m matrix)]
@interaction[(print-m (transpose matrix))]