#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Lijsten met procedures en symbolen}

Indien p, q en r als volgt gedefinieerd zijn:
@defs+int[((define p (list cons +))
           (define q '(cons +))
           (define r (list 'cons '+)))]
Wat is dan het resultaat van volgende expressies ?
@itemize{@item[@predict[((car p)  3 4)]]
          @item[@predict[((cadr p) 3 4)]]
          @item[@predict[((car r)  3 4)]]
          @item[@predict[((cadr q) 3 4)]]}