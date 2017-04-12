#lang racket
(require scheme/sandbox scribble/core scribble/decode
         scribble/html-properties scribble/latex-properties
         (prefix-in eval: scribble/eval)
         scribble/manual scribble/html-properties racket/system
         "parameter.rkt"
         "math-utilities.rkt")



(provide
 solution 
 reset-r5rs-evaluator
 interaction def+int defs+int hidden-code just-eval
 predict
 setup-math math-in math-disp)


;-------------------------------------------------------------------------------


(define solution-style
  (make-style "solution"
              (list (make-css-addition "solution.css")
                    (make-tex-addition "solution.tex"))))

(define (solution-note . c)
  (nested #:style solution-style c))

(define (solution . args)
  (when (show-solution?) (apply solution-note args)))

;-------------------------------------------------------------------------------
(define the-evaluator 'not-set)

(define (reset-r5rs-evaluator)
  (set! the-evaluator (make-r5rs-evaluator)))

(define (make-r5rs-evaluator)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (make-evaluator 'r5rs
                    '(#%require (only racket/base current-print void? error))
                    '(current-print
                      (lambda (v)
                        (if (not (void? v))
                            (display v)))))))

(reset-r5rs-evaluator)

;-------------------------------------------------------------------------------
(define-syntax interaction
  (syntax-rules ()
    ((interaction datum ...)(eval:interaction #:eval the-evaluator datum ...))))

(define-syntax def+int
  (syntax-rules ()
    ((def+int defn-datum expr-datum ...)(eval:def+int #:eval the-evaluator defn-datum expr-datum ...))))

(define-syntax defs+int
  (syntax-rules ()
    ((defs+int (defn-datum ...) expr-datum ...)
     (eval:defs+int #:eval the-evaluator (defn-datum ...) (quote expr-datum) ...))))

(define (just-eval code)(the-evaluator code))

(define-syntax hidden-code
  (syntax-rules ()
    ((hidden-code datum)(the-evaluator (quote datum)))))

;-------------------------------------------------------------------------------
(define-syntax predict
  (syntax-rules ()
    ((predict expr)
     (if (show-solution?)
         (interaction expr)
         (scheme expr)))))

;-------------------------------------------------------------------------------

