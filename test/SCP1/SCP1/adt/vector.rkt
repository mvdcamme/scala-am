#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{ADT Wiskundige Vectoren}
Definieer een ADT voor wiskundige vectoren.
Schrijf hiervoor de constructor @scheme[(make-vector dim)] en de selectoren
@scheme[(coordinate vec i)] en @scheme[(dimension vec)].

@section{Lijst}
Implementeer dit aan de hand van lijsten.

@solution[@defs+int[((define (make-vector-l . coords) 
                       coords)
                     (define (dimension-l vec) (length vec))
                     (define (coordinate-l vec i) (list-ref vec i)))]]



@interaction[(define v-l (make-vector-l 1 2 3))
             v-l
             (dimension-l v-l)
             (coordinate-l v-l 1)]

@section{Vector}
Implementeer dit aan de hand van Scheme-vectoren.
@solution[@defs+int[((define (make-vector-v . coords) 
                       (list->vector coords))
                     (define (dimension-v vec) (vector-length vec))
                     (define (coordinate-v vec i) (vector-ref vec i)))]]


@interaction[(define v-v (make-vector-v 1 2 3))
             v-v
             (dimension-v v-v)
             (coordinate-v v-v 1)]

@section{Performantie}
Vergelijk de performantie van beide implementaties.

@solution{De constructor voor de lijst versie is in O(1),
          de constructor voor de vector versie in O(n) omdat @scheme[list->vector]
          over de volledige lijst moet lopen.
          
          In het geval van de selectoren is dit omgekeerd: daar is de lijst implementatie
          van zowel @scheme[(dimension vec)] als @scheme[(coordinate vec i)] in O(n) door
          het gebruik van respectievelijk @scheme[lenght] en @scheme[list-ref] die zelf in O(n) werken.
          De vector implementatie werkt in O(1), omdat @scheme[vector-length] en
          @scheme[vector-ref] in O(1) werken.}


@section{Reageren op onverwachte input}
Wat zul je doen met coördinaten die niet bestaan?

@solution{We kunnen er voor kiezen om bij het opvragen van coördinaten
          die niet bestaan, @scheme[#f] terug te geven.
          Merk op dat we onze oude implementatie hergebruiken.
          @def+int[(define (coordinate-e vec i)
                     (if (or (< i 0) (>= i (dimension-l vec)))
                         #f
                         (coordinate-l vec i)))]
          
          @interaction[(define v-e (make-vector-l 1 2 3))
                       (coordinate-e v-e -1)
                       (coordinate-e v-e  0)
                       (coordinate-e v-e  1)
                       (coordinate-e v-e  2)
                       (coordinate-e v-e  3)]}

@section{Bewerkingen}
Definieer het optellen, aftrekken, de scalaire vermenigvuldiging, en het inproduct voor vectoren.
Bemerk de opvallende gelijkheid tussen het optellen en aftrekken van vectoren.
Hoe kan je dit laatste in je voordeel gebruiken?
Hint: Je mag er in je oplossing van uit gaan dat vectoren dezelfde dimensie hebben.
@solution[@defs+int[((define (vector->list-v vec)
                       (let loop ((i 0))
                         (if (< i (dimension-v vec))
                             (cons (coordinate-v vec i) (loop (+ i 1)))
                             '())))
                     
                     (define (vector-som v1 v2)
                       (apply make-vector-v 
                              (map + 
                                   (vector->list-v v1)
                                   (vector->list-v v2)))))]]


@interaction[(define v1-v (make-vector-v 1 2 3))
             (define v2-v (make-vector-v 4 5 6))
             (vector-som v1-v v2-v)]

@solution[@def+int[(define (vector-scalair-product v s)
                     (apply make-vector-v 
                            (map (lambda (c) (* c s)) 
                                 (vector->list-v v))))]]


@interaction[(define vs-v (make-vector-v 1 2 3))
             (vector-scalair-product vs-v 2)]

@solution[@def+int[(define (vector-verschil v1 v2)
                     (apply make-vector-v 
                            (map +
                                 (vector->list-v v1)
                                 (vector->list-v (vector-scalair-product v2 -1)))))]]


@interaction[(define vv1-v (make-vector-v 4 5 6))
             (define vv2-v (make-vector-v 1 2 3))
             (vector-verschil vv1-v vv2-v)]

@solution{@def+int[(define (vector-inproduct v1 v2)
                     (apply + 
                            (map * 
                                 (vector->list-v v1)
                                 (vector->list-v v2))))]}


@interaction[(define vi1-v (make-vector-v 4 5 6))
             (define vi2-v (make-vector-v 1 2 3))
             (vector-inproduct vi1-v vi2-v)]

@section{Punt als vector}
Wiskundige vectoren kunnen eigenlijk ook beschouwd worden als punten in een n-dimensionale ruimte.
In het bijzonder kunnen vectoren van lengte 2 beschouwd worden als punten in een 2-dimensionale ruimte.
Maak gebruik van deze kennis om de punten uit vraag 1 te implementeren d.m.v.
de wiskundige vectoren uit deze oefening.
Moet er veel veranderd worden aan de code?

@solution[@defs+int[((define (make-punt x y) (make-vector-v x y))
                     (define (x punt) (coordinate-v punt 0))
                     (define (y punt) (coordinate-v punt 1)))]]

