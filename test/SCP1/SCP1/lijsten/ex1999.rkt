#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title{Examen Informatica eerste zit 1999: Lijst vraag}
@section[#:tag "vraag1"]{Dissectie van een lijst}
Schrijf een procedure @scheme[(dissect-n lst n)] die een lijst dissecteert in een nieuwe lijst door afwisselend n elementen dan weer wel en dan weer niet te behouden. Indien er minder dan n elementen overblijven in de lijst, neem je alles wat er overblijft.

@solution{@def+int[(define (dissect-n lst n)
                     (define (geef-n+rest lst n)
                       (cond ((or (= 0 n) (null? lst)) (cons '() lst))
                             (else (let* ((res (geef-n+rest (cdr lst) (- n 1 )))
                                          (first (car res))
                                          (rest (cdr res)))
                                     (cons (cons (car lst) first) rest)))))
                     
                     (cond ((null? lst) '())
                           (else (let* ((n-lst (geef-n+rest lst n))
                                        (2n-lst (geef-n+rest (cdr n-lst) n))) 
                                   (append (car n-lst)
                                           (dissect-n (cdr 2n-lst) n))))))]}

@interaction[(dissect-n '(1 2 3 4 5) 2)
             (dissect-n '(1 2 3 4 5 6 7 8) 3)
             (dissect-n '(1 2) 4)]


@section{Recursie/iteratie}
Genereren de procedures in je oplossing recursieve of iteratieve processen? Herschrijf je oplossing uit @secref{vraag1} zodat je procedures een proces van het andere type genereren.

@solution{Bovenstaande procedures genereren recursieve processen}

@solution{@def+int[(define (dissect-n lst n) 
                     (define (geef-n+rest lst n)
                       (define (iter lst n res)
                         (cond ((or (null? lst) (= 0 n)) (cons res lst))
                               (else
                                (iter (cdr lst) (- n 1 ) (append res (list (car lst)))))))
                       (iter lst n '()))
                     
                     (define (iter lst result)
                       (cond ((null? lst) result)
                             (else (let* ((n-lst (geef-n+rest lst n))
                                          (2n-lst (geef-n+rest (cdr n-lst) n)))
                                     (iter (cdr 2n-lst) (append result (car n-lst)))))))
                     
                     (iter lst '()))]}


@section[#:tag "vraag3"]{Splitsen van een lijst}
Schrijf een procedure @scheme[(split lst n)] die niet alleen de gedissecteerde lijst uit @secref{vraag1} teruggeeft, maar ook een lijst die opgebouwd is uit de resten van de oorspronkelijke lijst.

@solution{@def+int[(define (split lst n)
                     (define (geef-n+rest lst n)
                       (cond ((or (= 0 n) (null? lst)) (cons '() lst))
                             (else (let* ((res (geef-n+rest (cdr lst) (- n 1 )))
                                          (first (car res))
                                          (rest (cdr res)))
                                     (cons (cons (car lst) first) rest)))))
                     
                     (cond ((null? lst) (list '() '()))
                           (else 
                            (let* ((n-lst (geef-n+rest lst n))
                                   (2n-lst (geef-n+rest (cdr n-lst) n))
                                   (split-rest (split (cdr 2n-lst) n))
                                   ;roep split-rest recusief op met de 
                                   ;lijst waarvan de 2n eerste elementen 
                                   ;verwijderd zijn. Je weet dat het 
                                   ;resultaat een lijst van 2 lijsten is.
                                   (first (car split-rest))
                                   (rest (cadr split-rest)))
                              (list (append (car n-lst) first)
                                    (append (car 2n-lst) rest))))))]}

@interaction[(split '(1 2 3 4 5 6 7 8 9 10) 2)]


@section{Splitsen van meerdere lijsten}
Veralgemeen de procedure uit @secref{vraag3} tot een procedure @scheme[(super-split lsts n)] die een willekeurig aantal lijsten split. De lijsten zitten samen in de formele paramater @scheme[lsts]. Als resultaat worden twee lijsten van lijsten teruggegeven. De eerste lijst bestaat uit alle gedissecteerde lijsten uit @secref{vraag1}, de tweede lijst bevat de restjes.

@solution{@def+int[(define (super-split lsts n)
                     (let ((first (map (lambda (l) (car (split l n))) lsts))
                           (rest (map (lambda (l) (cadr (split l n))) lsts)))
                       (list first rest)))]}

@interaction[(super-split '((a b c d e f) (g h i j k) (l m n o p q) (r s t u v w)) 3)]
