#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Oefening 3.6 uit Abelson&Sussman}
Pas de procedure @scheme[make-random] aan zodanig dat het mogelijk is om de
random generator te resetten. Bvb. indien @scheme[random] een random-generator
is die gedefinieerd is m.b.v. @scheme[make-random], dan produceert
@scheme[((random 'generate))] een nieuw random getal en
@scheme[((random 'reset) new-seed)] zet de interne variabele op de waarde @scheme[new-seed].

@hidden-code[(define (round-decimal x n)
               (exact->inexact (/ (round (* x (expt 10 n)))(expt 10 n))))]

@solution{@def+int[(define (make-random m a seed)
                     (let ((xi seed))
                       (define (reset new-seed)
                         (set! xi new-seed))
                       (define (generate)
                         (set! xi (modulo (* xi a) m))
                         (round-decimal (/ xi m) 3))
                       (lambda (msg)
                         (cond ((eq? msg 'generate)generate)
                               
                               ((eq? msg 'reset)reset)))))]}

@interaction[(define my-random2 (make-random (- (expt 2 32) 1) (expt 7 5) 99))
             (for-each (lambda (res)(display res)(newline))
                       (list ((my-random2 'generate))
                             ((my-random2 'generate))
                             ((my-random2 'generate))
                             ((my-random2 'reset) 99)
                             ((my-random2 'generate))
                             ((my-random2 'generate))
                             ((my-random2 'generate))))]