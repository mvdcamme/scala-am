#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@(parameterize ((current-input-port (open-input-file "streams/streams.rkt")))
   (let loop ((r (read)))
     (when (not (eof-object? r))
       (just-eval r) 
       (loop (read)))))

@setup-math

@title[#:tag-prefix "streams"]{Reeksontwikkelingen}
Maak gebruik van streams om de volgende reeksontwikkelingen tot op n termen te berekenen:

@itemize[#:style 'ordered]{
                           @item{@math-disp{e = \frac{1}{0!} +\frac{1}{1!} +\frac{1}{2!} +\frac{1}{3!} + ...}
                                  @solution{@defs+int[((define (create-stream a b)
                                                         (if (> a b)
                                                             the-empty-stream
                                                             (cons-stream a (create-stream (+ a 1) b))))
                                                       
                                                       (define (fac n) (if (<= n 0) 1 (* n (fac (- n 1)))))
                                                       
                                                       (define (e n)
                                                         (accumulate + 0 (map-stream (lambda (x) (/ 1 (fac x)))
                                                                                     (create-stream 0 n)))))]
                                             
                                             @predict[(exact->inexact (e 10))]}}
                            @item{@math-disp{sin(x)=\frac{x}{1!} - \frac{x^3}{3!} + \frac{x^5}{5!} - \frac{x^7}{7!} + ...}
                                   @solution{@defs+int[((define (sinus x)
                                                          (define (stream-sinus x n)
                                                            (define (calc-term t)
                                                              (if (odd? (/ (- t 1) 2))
                                                                  (* -1 (/ (expt x t) (fac t)))
                                                                  (/ (expt x t) (fac t))))
                                                            (accumulate + 
                                                                        0 
                                                                        (map-stream calc-term 
                                                                                    (streamfilter odd?
                                                                                                  (enumerate-interval 0 n)))))
                                                          (stream-sinus x 10)))]
                                              @predict[(sinus (/ 3.1415 2))]}}}

