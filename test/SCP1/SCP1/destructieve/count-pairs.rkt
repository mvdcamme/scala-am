#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Correcte versie @scheme[count-pairs]}
Schrijf een correcte versie van de procedure @scheme[count-pairs] die het aantal verschillende cons-cellen teruggeeft.

@bold{Hint}: hou een extra lijst bij van reeds getelde cons-cellen.

@solution{@defs+int[((define (count-pairs lst)
                       (let ((path '()))
                         (define (count current)
                           (cond 
                             ((null? current) 0)
                             ((not (pair? current)) 0)
                             ((memq current path) 0)
                             (else 
                              (set! path (cons current path))
                              (+ 1 (count (car current))
                                 (count (cdr current))))))
                         (count lst))))]}

@itemize[#:style 'ordered]{@item{@def+int[(define ret3 (cons 'a (cons 'b (cons 'c '()))))]
                                 @interaction[(count-pairs ret3)]}
                            @item{@def+int[(define ret4
                                             (let ((last (cons 'c '())))
                                               (cons last (cons 'b last))))]
                                   @interaction[(count-pairs ret4)]}
                            @item{@def+int[(define ret7
                                             (let* ((last (cons 'c '()))
                                                    (middle (cons last last)))
                                               (cons middle middle)))]
                                   @interaction[(count-pairs ret7)]}
                            @item{@def+int[(define retno
                                             (let* ((last (cons 'c '()))
                                                    (lst (cons 'a (cons 'b last))))
                                               (set-cdr! last lst)
                                               lst))]
                                   @interaction[(count-pairs retno)]}}


