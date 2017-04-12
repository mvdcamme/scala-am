#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@hidden-code[(define (atom? x) (not (pair? x)))]


@title{Same Structure}
Definieer het predicaat @scheme[(same-structure? l1 l2)],
dat nagaat of twee lijsten op de atomen na dezelfde structuur hebben.
Hou rekening met dotted-pair.

@solution[@defs+int[((define (same-structure? l1 l2)
                       (cond ((and (atom? l1) (atom? l2)) #t)
                             ((or  (atom? l1) (atom? l2)) #f)
                             (else (and (same-structure? (car l1) (car l2))
                                        (same-structure? (cdr l1) (cdr l2))))))
                     
                     (define (same-structure?-or l1 l2)
                       (or (and (atom? l1) (atom? l2))
                           (and (pair? l1)
                                (pair? l2)
                                (same-structure?-or (car l1) (car l2))
                                (same-structure?-or (cdr l1) (cdr l2))))))]]


@interaction[(same-structure? '((1 2) ((3 . 4) ((5 6) ((7 8) (9)))))
                              '((a b) ((c . d) ((e f) ((g h) (i))))))
             (same-structure? '((1 2) ((3 4) ((5 6) ((7 8) (9)))))
                              '((((1 2) (3 4)) ((5 6) (7 8))) 9))]

