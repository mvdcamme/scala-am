#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Oefening 3.18 uit Abelson&Sussman}
Schrijf een predicaat @scheme[(cycles? r)] dat zegt of @scheme[r] een cyclus bevat (d.w.z. dat het aflopen van de lijst door opeenvolgende cdr's in een oneindige loop raakt).

@solution{@defs+int[((define (cycles? lst)
                       (define (find-cycles? current path)
                         (cond 
                           ((null? current) #f)
                           ((memq current path) #t)
                           (else (find-cycles? (cdr current)
                                               (cons current path)))))
                       (find-cycles? lst '())))]}

@interaction[(cycles? '())]
@interaction[(cycles? '(1 2 3))]
@interaction[(define ret4
               (let ((last (cons 'c '())))
                 (cons last (cons 'b last))))]
@interaction[(cycles? ret4)]
@interaction[(define retno
               (let* ((last (cons 'c '()))
                      (lst (cons 'a (cons 'b last))))
                 (set-cdr! last lst)
                 lst))]
@interaction[(cycles? retno)]
@interaction[(define ret7
               (let* ((last (cons 'c '()))
                      (middle (cons last last)))
                 (cons middle middle)))]
@interaction[(cycles? ret7)]
@interaction[(cycles? (cons 'a (cons 'b retno)))]
