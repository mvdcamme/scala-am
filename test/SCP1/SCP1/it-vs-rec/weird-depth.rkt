#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Diepte van een recursief proces}

De (wiskundige-)functie weird is als volgt gedefinieerd:
@math-disp{weird(x) = \left\lbrace\begin{array}{ll}  1  & \mbox{als } x=1 \\ weird (x/2) & \mbox{als  x even is}\\ weird (3*x+1) & \mbox{anders}\end{array}\right.}.


@section{Implementeer}
Schrijf de procedure @scheme[(weird x)] die de wiskundige functie
van hierboven implementeert.

@solution[@def+int[(define (weird x)
                     (cond
                       ((= x 1) 1)
                       ((even? x) (weird (/ x 2)))
                       (else (weird (+ (* 3 x) 1)))))]]

@interaction[(weird 15)]

@section{Bereken de recursie diepte}
Schrijf de procedure @scheme[(depth-weird x)] die het aantal recursieve
oproepen (= de recursie-diepte) van weird voor een bepaalde x teruggeeft.

@solution[@def+int[(define (depth-weird x)
                     (cond
                       ((= x 1) 0)
                       ((even? x) (+ 1 (depth-weird (/ x 2))))
                       (else (+ (depth-weird (+ (* 3 x) 1)) 1))))]]

@interaction[(depth-weird 15)]

@section{Tabel}
Schrijf een procedure @scheme[(weird-table min max)] 
die op het scherm een tabel afdrukt van de recursie-dieptes 
van weird voor alle getallen gelegen tussen min en max.

@solution[@def+int[(define (weird-table min max)
                     (cond
                       ((< min max)
                        (for-each display (list min "\t" (depth-weird min) "\n"))
                        (weird-table (+ min 1) max))))]]

@interaction[(weird-table 1 10)]
