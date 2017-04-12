#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen januari 2003: Object vraag}
Ontwerp 2 ADT's die je als object implementeert.

@section{Tel-object}
Het eerste ADT stelt een tel-object voor. Met dit object is het mogelijk cijfers in te toetsen, die geaccumuleerd worden tot 1 som. Dit bedrag moet nadien kunnen gereset worden. De messages die je moet voorzien zijn de volgende:
@itemize[#:style 'ordered]{
                           @item{@bold{@scheme[(toets bedrag)]}: bedrag wordt ingetoetst in het tel-object en opgeteld bij het huidige bedrag.}
                            @item{@bold{@scheme[(lees)]}: geeft het huidige bedrag.}
                            @item{@bold{@scheme[(reset)]}: het huidige bedrag wordt op nul gezet.}}

@solution{@defs+int[((define (maak-teller)
                       (let ((result 0))
                         
                         (define (toets bedrag)
                           (set! result (+ result bedrag)))
                         
                         (define (reset)
                           (set! result 0))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'toets) toets)
                                 ((eq? msg 'lees) result)
                                 ((eq? msg 'reset) (reset))
                                 (else (error "wrong message"))))
                         dispatch)))]
           
           @interaction[(define teller (maak-teller))]
           @interaction[((teller 'toets) 20)]
           @interaction[(teller 'lees)]
           @interaction[(teller 'reset)]
           @interaction[(teller 'lees)]}



@section{Winkelkassa}
Het tweede ADT omvat de werking van een winkelkassa. In een winkelkassa moet je bedragen kunnen intoetsen die geaccumuleerd worden tot 1 som, dit is het bedrag dat 1 klant moet betalen. Het bedrag dat de klant overhandigt moet eveneens ingetoetst worden; het wisselgeld moet worden teruggegeven en het saldo van de kassa moet verhoogd worden. Op elk moment moet het saldo van de kassa kunnen opgevraagd worden en op het einde van de dag moet de kassa afgesloten worden en het saldo gereset. De volgende messages moet je voorzien:
@itemize[#:style 'ordered]{
                           @item{@bold{@scheme[(toets bedrag)]}: een bedrag wordt ingetoetst in de kassa, dit kan zowel voor de kostprijs van 1 artikel zijn, als voor het intoetsen van geld dat de klant overhandigt. Let wel het intoetsen van het overhandigde bedrag kan pas gebeuren als de totale kostprijs werd afgelosten m.b.v. de @scheme[enter] message.}
                            @item{@bold{@scheme[(enter)]}: afhankelijk van wat werd ingetoetst reageert de kassa als volgt: wanneer de prijs van het laatste artikel werd ingetoetst, wordt het totale bedrag dat de klant moet betalen afgesloten en teruggegeven, wanneer het bedrag werd ingetoetst dat de klant overhandigt dan wordt het wisselgeld berekend, het saldo van de kassa wordt verhoogd met het verschuldigde bedrag, het verschuldigde bedrag wordt gereset op nul en uiteindelijk wordt het wisselgeld teruggegeven.}
                            @item{@bold{@scheme[(inhoud)]}: het huidige saldo van de kassa wordt opgevraagd.}
                            @item{@bold{@scheme[(afsluiten)]}: het huidige saldo van de kassa wordt teruggegeven en op nul gereset.}}

Laat door middel van een klein transcriptje zien hoe je een winkelkassa aanmaakt en gebruikt.

@solution{@defs+int[((define (maak-winkelkassa)
                       (let ((saldo (maak-teller))
                             (te-betalen (maak-teller))
                             (ingetoetst 'product)
                             (ontvangen 0))
                         
                         (define (toets type bedrag)
                           (set! ingetoetst type)
                           (cond  ((eq? type 'product)
                                   ((te-betalen 'toets) bedrag))
                                  ((eq? type 'ontvangen)
                                   (set! ontvangen bedrag))
                                  (else (error "wrong type"))))
                         
                         (define (enter)
                           (if (eq? ingetoetst 'product)
                               (te-betalen 'lees)
                               (let ((wisselgeld (- ontvangen (te-betalen 'lees))))
                                 ((saldo 'toets) (te-betalen 'lees))
                                 (te-betalen 'reset)
                                 wisselgeld)))
                         
                         (define (inhoud)
                           (saldo 'lees))
                         
                         (define (afsluiten)
                           (let ((teruggeven saldo))
                             (set! saldo 0)
                             teruggeven))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'toets) toets)
                                 ((eq? msg 'enter) (enter))
                                 ((eq? msg 'inhoud) (inhoud))
                                 ((eq? msg 'afsluiten) (afsluiten))
                                 (else (error "wrong message"))))
                         dispatch)))]
           
           @interaction[(define winkelkassa (maak-winkelkassa))]
           @interaction[((winkelkassa 'toets) 'product 20)] 
           @interaction[((winkelkassa 'toets) 'product 5)]
           @interaction[(winkelkassa 'enter)]
           @interaction[((winkelkassa 'toets) 'product 10)]
           @interaction[(winkelkassa 'enter)]
           @interaction[((winkelkassa 'toets) 'ontvangen 50)]
           @interaction[(winkelkassa 'enter)]
           @interaction[(winkelkassa 'inhoud)]}