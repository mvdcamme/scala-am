#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Binaire vormen}

Schrijf een procedure @scheme[(display-as-binary n)], die 
een positief geheel getal n neemt, en dit in binaire 
vorm afdrukt. De meest rechtse bit is 1 als het getal
oneven is, 0 als het getal even is. Voor de tweede bit
van rechts deel je het getal door 2 (met de functie
@scheme[(quotient n1 n2)] die het geheel equivalent is van @scheme[(/ n1 n2)],
en je doet dezelfde test. Voor de derde bit van rechts deel
je het vorige quotiënt door 2 enz. ...
Je mag een recursief proces genereren.
Gebruik @scheme[(display "0")] en @scheme[(display "1")] om de cijfers 0 en 1
af te drukken. Let op dat je het getal niet omgekeerd afdrukt.
Enkele correcte voorbeelden zijn:

@solution[@def+int[(define (display-as-binary n)
                     (if (> n 1)
                         (display-as-binary (quotient n 2)))
                     (display (modulo n 2)))]]

@interaction[(display-as-binary 8)
             (display-as-binary 5)
             (display-as-binary 0)]
