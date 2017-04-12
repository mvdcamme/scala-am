#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Examen Informatica Partieel januari 1996}
Een gloeilamp heeft slechts 3 mogelijke toestanden: "aan", "uit" of "kapot".
Initieel is een gloeilamp steeds "uit". Elke gloeilamp gaat na een bepaalde tijd kapot.
Voor gloeilampjes in een kerstboom wordt deze tijd voornamelijk bepaald door het aantal ker dat het lampje al aan en uit is gegaan. Het lampje kan nadat het kapot is gegaan wel vervangen worden en gaat vanzelf dan weer een bepaalde tijd mee.

Schrijf een Scheme-procedure @scheme[(MaakLampje aantal)] die dergelijke gloeilampjes aanmaakt. De parameter @scheme[aantal] die moet meegegeven worden bepaalt het aantal keer dat het lampje van toestand kan veranderen alvorens kapot te gaan. Een gloeilamp aangemaakt door deze procedure is een object dat de volgende boodschappen moet verstaan:

@itemize[#:style 'ordered]{
                           @item{@bold{switch!} Verandert de toestand van de gloeilamp van "aan" naar "uit" of omgekeerd. Geeft na afloop @scheme[#t] terug indien het lampje nog steeds niet kapot is, anders @scheme[#f].}
                            @item{@bold{on?} Geeft @scheme[#t] indien de gloeilamp "aan" is, zoniet @scheme[#f].}
                            @item{@bold{off?} Geeft @scheme[#t] indien de gloeilamp "uit" is, zoniet @scheme[#f].}
                            @item{@bold{test?} Geeft @scheme[#t] indien de gloeilamp "kapot" is, zoniet @scheme[#f].}
                            @item{@bold{change!} Vervangt de gloeilamp zodat ze weer een aantal keer aan en uit kan gaan. Deze tijd wordt bepaald door een parameter @scheme[nieuwAantal] die als argument moet worden meegegeven aan de boodschap. Na afloop wordt het symbool @scheme['changed] teruggegeven.}}

@solution{@defs+int[((define (MaakLampje aantal)
                       (define state 'off)
                       
                       (define (on!) (set! state 'on))
                       
                       (define (off!) (set! state 'off))
                       
                       (define (broken!) (set! state 'broken))
                       
                       (define (on?) (eq? state 'on))
                       
                       (define (off?) (eq? state 'off))
                       
                       (define (broken?) (eq? state 'broken))
                       
                       (define (switch!)
                         (set! aantal (- aantal 1))
                         (cond ((< aantal 0) (broken!))
                               ((off?) (on!))
                               ((on?) (off!)))
                         (not (broken?)))
                       
                       (define (change! nieuw)
                         (off!)
                         (set! aantal nieuw)
                         'changed)
                       
                       (define (dispatch msg)
                         (cond ((eq? msg 'switch!) (switch!))
                               ((eq? msg 'on?) (on?))
                               ((eq? msg 'off?) (off?))
                               ((eq? msg 'test?) (broken?))
                               ((eq? msg 'change!) change!)
                               (else (error "Message not understood."))))
                       dispatch))]
           
           @interaction[(define philips (MaakLampje 5))]  
           @interaction[(philips 'test?)]
           @interaction[(philips 'on?)]                  
           @interaction[(philips 'off?)]
           @interaction[(philips 'switch!)]
           @interaction[(philips 'switch!)]
           @interaction[(philips 'switch!)]
           @interaction[(philips 'switch!)]
           @interaction[(philips 'switch!)]
           @interaction[(philips 'switch!)]
           @interaction[(philips 'test)]
           @interaction[(philips 'test?)]
           @interaction[((philips 'change!) 10)]
           @interaction[(philips 'test?)]
           @interaction[(philips 'off?)]}





