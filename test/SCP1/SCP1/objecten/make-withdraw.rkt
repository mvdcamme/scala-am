#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{Make-withdraw}
In de @scheme[make-withdraw] procedure uit het boek wordt de interne variabele @scheme[balance] gecreëerd als een parameter van @scheme[make-withdraw]. Het is ook mogelijk om dergelijke interne variabelen expliciet te creëren met een @scheme[let].

@schemeblock[(define (make-withdraw initial-amount)
               (let ((balance initial-amount))
                 (lambda (amount)
                   (if (>= balance amount)
                       (begin (set! balance (- balance amount))
                              balance)
                       "Insufficient funds"))))]

Gebruik omgevingsmodel-diagrammen om deze versie van @scheme[make-withdraw] te analyseren.
Bespreek de verschillen met de versie uit het boek.

@solution[ @image["objecten/images/make-withdraw/mw-box5.png"] ]

@solution{Deze oplossing lijkt sterk op de make-withdraw uit het boek, en dat is ook de bedoeling. Het grote verschil is dat de lokale variabelen nu aangemaakt worden door de body, zodat we eventueel meer 'state' kunnen aanmaken dat het aantal parameters in de procedure. We kunnen ook nog altijd de startwaarde van de balans manipuleren.}

