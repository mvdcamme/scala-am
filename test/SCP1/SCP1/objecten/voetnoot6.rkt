#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@setup-math


@title{Voetnoot 6 blz.226 uit Abelson&Sussman}
Schrijf een procedure @scheme[(make-random m a seed)] die onafhankelijke
random generatoren creëert. De random getallen worden gegenereerd met de reeks
@math-in{x_i+1 = (x_i * a) \mod m}, waarbij @math-in{x_0 = seed} (goede waarden voor m en
a zijn @math-in{2^{32} -1} en @math-in{a = 7^5}). Om de gewenste output te
bekomen moet je enkel nog @math-in{\frac{x_i}{m}} uitwerken zodat je een getal tussen 0 en 1
bekomt.

@hidden-code[(define (round-decimal x n)
  (exact->inexact (/ (round (* x (expt 10 n)))(expt 10 n))))]

@solution{@def+int[(define (make-random m a seed)
                     ; Deze procedure maakt een random-generator die 
                     ; willekeurige getallen tussen 0 en 1 oplevert.
                     (let ((xi seed))
                       (lambda ()
                         (set! xi (modulo (* xi a) m))
                         (round-decimal (/ xi m) 3))))]}

De procedure zal bijvoorbeeld als volgt gebruikt kunnen worden:
@interaction[(define random (make-random (- (expt 2 32) 1) (expt 7 5) 97))
             (random)
             (random)
             (random)]



