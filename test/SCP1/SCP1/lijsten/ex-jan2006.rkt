#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica januari 2006}
@section[#:tag "vraag1"]{Comprimeer}
Schrijf een procedure @scheme[(comprimeer metingen)] die een lijst temperatuursmetinegn als parameter neemt en een nieuwe lijst teruggeeft waarin opeenvolgende gelijke metingen verkort worden voorgesteld. Dit gebeurt door een lijstje van 2 getallen. Het eerste getal is de eigenlijke temperatuursmeting, het tweede getal is het aantal maal dat deze meting achtereenvolgens werd waargenomen.

@solution[@def+int[(define (comprimeer metingen)
                     (define (hulp lst prev count)
                       (cond ((null? lst) (list (list prev count)))
                             ((= (car lst) prev) 
                              (hulp (cdr lst) prev (+ count 1)))
                             (else (cons (list prev count) (hulp (cdr lst) (car lst) 1)))))
                     (if (null? metingen)
                         '()
                         (hulp (cdr metingen) (car metingen) 1)))]]

@interaction[(comprimeer '(37.5 37.5 37.2 38.0 38.0 38.0 38.3))]

@section{Recursie/iteratie}
Genereren de procedures in je oplossing recursieve of iteratieve processen? Herschijf je oplossing uit @secref{vraag1} zodat je procedures een proces van het andere type genereren.

@solution[@def+int[(define (comprimeer-iter metingen)
                     (define (hulp lst prev count res)
                       (cond ((null? lst) (reverse (cons (list prev count) res)))
                             ((= (car lst) prev)
                              (hulp (cdr lst) prev (+ count 1) res))
                             (else (hulp (cdr lst) 
                                         (car lst) 
                                         1 
                                         (cons (list prev count) res)))))
                     (if (null? metingen)
                         '()
                         (hulp (cdr metingen) (car metingen) 1 '())))]]

@interaction[(comprimeer-iter '(37.5 37.5 37.2 38.0 38.0 38.0 38.3))]


@section{Algemene versie voor comprimeren}
Veralgemeen de procedure uit @secref{vraag1} tot een procedure @scheme[(comprimeer-algemeen lst op)] die dezelfde compressie-operatie doet, maar nu een extra parameter op neemt, d.i. de operator waarmee de elementen op gelijkheid getest moeten worden.

@solution[@def+int[(define (comprimeer-algemeen metingen test)
                     (define (hulp lst prev count res)
                       (cond ((null? lst) (reverse (cons (list prev count) res)))
                             ((test (car lst) prev)
                              (hulp (cdr lst) prev (+ count 1) res))
                             (else (hulp (cdr lst) 
                                         (car lst) 
                                         1 
                                         (cons (list prev count) res)))))
                     (if (null? metingen)
                         '()
                         (hulp (cdr metingen) (car metingen) 1 '())))]]


@section{Herschrijf}
Herschrijf m.b.b. de procedure @scheme[comprimeer-algemeen] de procedure @scheme[comprimeer] uit @secref{vraag1}. Schrijf eveneens d.m.v. @scheme[comprimeer-algemeen] een procedure @scheme[(comprimeer-zonder-decimalen metingen)] die dezelfde compressie-operatie uit @secref{vraag1} uitvoert, maar nu 2 temperatuursmetingen als gelijk beschouwt als het gehele deel voor de komma gelijk is. 

@solution[@defs+int[((define (comprimeer metingen)
                       (comprimeer-algemeen metingen (lambda (x y) (= x y))))
                     
                     (define (comprimeer-zonder-decimalen metingen)
                       (let ((getallen (map floor metingen)))
                         (comprimeer-algemeen getallen (lambda (x y) (= x y))))))]]

@interaction[(comprimeer '(37.5 37.5 37.2 38.0 38.0 38.0 38.3))]
@interaction[(comprimeer-zonder-decimalen '(37.5 37.5 37.2 38.0 38.0 38.0 38.3))]
             
             
             