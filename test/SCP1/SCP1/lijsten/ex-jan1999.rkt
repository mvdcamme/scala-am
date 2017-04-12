#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")

@title[#:tag "vraag1"]{Examen Informatica Partieel januari 1999: Lijst vraag}
@section{Samenvoegen van twee lijsten}
Schrijf een procedure @scheme[(merge-n lst1 lst2 n)] die 2 lijsten samenvoegt tot 1 enkele lijst door telkens n elementen van de eerste lijst te laten volgen door n elementen van de tweede lijst, enzovoort. Indien er minder dan n elementen overblijven in een lijst, neem je alles wat er overblijft.

@solution{@def+int[(define (merge-n lst1 lst2 n)
                     
                     (define (geef-n+rest lst n)
                       (cond ((or (= 0 n) (null? lst)) (cons '() lst))
                             (else (let* ((res (geef-n+rest (cdr lst) (- n 1 )))
                                          (first (car res))
                                          (rest (cdr res)))
                                     (cons (cons (car lst) first) rest)))))
                     
                     (cond ((null? lst1 ) lst2)
                           ((null? lst2) lst1)
                           (else
                            (let* ((n-lst1 (geef-n+rest lst1 n))
                                   (n-lst2 (geef-n+rest lst2 n))
                                   (n-lst1-first (car n-lst1))
                                   (n-lst1-rest (cdr n-lst1))
                                   (n-lst2-first (car n-lst2))
                                   (n-lst2-rest (cdr n-lst2)))
                              (append (append n-lst1-first n-lst2-first)
                                      (merge-n n-lst1-rest n-lst2-rest n))))))]}

@interaction[(merge-n '(1 2 3 4 5) '(6 7 8 9) 2)
             (merge-n '(1 2 3 4 5) '(6 7 8 9) 3)
             (merge-n '(1 2) '(3 4 5 6) 4)]

@section{Recursie/iteratie}
Genereren de procedures in je oplossing recursieve of iteratieve processen? Kan je de oplossing uit @secref{vraag1} dan herschrijven zodat je procedures een proces van het andere type genereren?

@solution{Zowel geef-n+rest als merge-n genereren een recursief proces. Nadat de laatste recursieve oproep gedaan is moet het resultaat nog opgebouwd worden (constructieve recursie).}

@solution{@def+int[(define (merge-n lst1 lst2 n)
                     
                     (define (geef-n+rest lst n)
                       (define (iter lst n res)
                         (cond ((or (null? lst) (= 0 n)) (cons res lst))
                               (else
                                (iter (cdr lst) 
                                      (- n 1 ) 
                                      (append res (list (car lst)))))))
                       (iter lst n '()))
                     
                     (define (iter-merge lst1 lst2 res)
                       (cond ((null? lst1) (append res lst2))
                             ((null? lst2) (append res lst1))
                             (else (let* ((n-lst1 (geef-n+rest lst1 n))
                                          (n-lst2 (geef-n+rest lst2 n))
                                          (n-lst1-first (car n-lst1))
                                          (n-lst1-rest (cdr n-lst1))
                                          (n-lst2-first (car n-lst2))
                                          (n-lst2-rest (cdr n-lst2)))
                                     (iter-merge n-lst1-rest 
                                                 n-lst2-rest 
                                                 (append res 
                                                         (append n-lst1-first 
                                                                 n-lst2-first)))))))
                     
                     (iter-merge lst1 lst2 '()))]}

@interaction[(merge-n '(1 2 3 4 5) '(6 7 8 9) 2)
             (merge-n '(1 2 3 4 5) '(6 7 8 9) 3)
             (merge-n '(1 2) '(3 4 5 6) 4)]

@section{Willekeurig aantal lijsten}
Veralgemeen de procedure uit @secref{vraag1} tot een procedure @scheme[(super-merge-n lsts n)] die een willekeurig aantal lijsten samenvoegt door achtereenvolgens n elementen van elke lijst te nemen. De lijsten zitten samen in de formele parameter @scheme[lsts].

@solution{@defs+int[((define (super-merge-n lsts n)
                      
                      (define (geef-n+rest lst n)
                        (cond ((or (= 0 n) (null? lst)) (cons '() lst))
                              (else (let* ((res (geef-n+rest (cdr lst) (- n 1)))
                                           (first (car res))
                                           (rest (cdr res)))
                                      (cons (cons (car lst) first) rest)))))
                      
                      (if (null? lsts)
                          '()
                          (let* ((g-n+rest (geef-n+rest (car lsts) n))
                                 (first (car g-n+rest))
                                 (rest (cdr g-n+rest)))
                            (append first
                                    (super-merge-n (append (cdr lsts) 
                                                           (if (null? rest)
                                                               rest
                                                               (list rest)))
                                                   n))))))]}


@interaction[(super-merge-n '((a b c d e f) 
                              (g h i j k)
                              (l m n o p q)
                              (r s t u v w))
                            3)]



