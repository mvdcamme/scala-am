#lang scribble/manual
@(require "../oefeningen-bundel-utils.rkt")


@title{ADT's en objecten}
Geef object-georiënteerde implementaties van de ADT's uit de reeks over Lijsten.
Bijvoorbeeld de ADT's Punt, Lijnstuk, Wiskundige Vector, Veelterm.

@solution{@defs+int[(; Object voor punt
                     (define (make-point x y)
                       
                       (define (dispatch msg)
                         (cond ((eq? msg 'x-value) x)
                               ((eq? msg 'y-value) y)
                               (else (error "wrong message"))))
                       dispatch)
                     
                     (define (make-segment start end)
                       
                       (define (midpoint)
                         (make-point (/ (+ (start 'x-value) (end 'x-value)) 2)
                                     (/ (+ (start 'y-value) (end 'y-value)) 2)))
                       
                       (define (dispatch msg)
                         (cond ((eq? msg 'start-point) start)
                               ((eq? msg 'end-point) end)
                               ((eq? msg 'midpoint) (midpoint))
                               (else (error "wrong message"))))
                       dispatch)
                     
                     ; Object voor wiskundige vector
                     (define (make-w-vector . args)
                       
                       (define (dimension)
                         (length args))
                       
                       (define (coordinate n)
                         (if (or (< n 1) (> n (dimension)))
                             (error "coordinate is out of range")
                             (list-ref args (- n 1))))
                       
                       (define (add w-vector)
                         (define (loop ctr res)
                           (if (= ctr 0)
                               (apply make-w-vector res)
                               (loop (- ctr 1) (cons (+ (coordinate ctr)
                                                        ((w-vector 'coordinate) ctr))
                                                     res))))
                         (loop (dimension) '()))
                       
                       ; Zo ook voor subtract, scalar-product, ...
                       
                       (define (dispatch msg)
                         (cond ((eq? msg 'dimension) (dimension))
                               ((eq? msg 'coordinate) coordinate)
                               ((eq? msg 'add) add)
                               (else (error "wrong message"))))
                       dispatch)
                     
                     ; Object voor veelterm
                     (define (make-polynome . coefficients)
                       (let ((polynome (apply make-w-vector coefficients)))
                         
                         (define (coefficient index)
                           ((polynome 'coordinate) index))
                         
                         (define (order)
                           (- (polynome 'dimension) 1))
                         
                         (define (dispatch msg)
                           (cond ((eq? msg 'order) (order))
                                 ((eq? msg 'coefficient) coefficient)
                                 (else (error "wrong message"))))
                         dispatch)))]}


@itemize[#:style 'ordered]{@item{@interaction[(define point1 (make-point 6 10))]}
             @item{@interaction[(define point2 (make-point 10 20))]}
             @item{@interaction[point1]}              
             @item{@interaction[(point1 'x-value)]}
             
             @item{@interaction[(define segment (make-segment point1 point2))]}
             @item{@interaction[segment]}  
             @item{@interaction[(segment 'start-point)]}     
             @item{@interaction[((segment 'start-point) 'y-value)]}   
             @item{@interaction[(define midpoint (segment 'midpoint))]}
             @item{@interaction[midpoint]}              
             @item{@interaction[(midpoint 'x-value)]}   
             
             @item{@interaction[(define w-vector1 (make-w-vector 1 2 3))]}
             @item{@interaction[(define w-vector2 (make-w-vector 4 5 6))]}
             @item{@interaction[((w-vector1 'coordinate) 2)]}    
             @item{@interaction[((w-vector2 'coordinate) 1)]}   
             @item{@interaction[((w-vector1 'add) w-vector2)]}   
             @item{@interaction[((((w-vector1 'add) w-vector2) 'coordinate) 1)]}
             
             @item{@interaction[(define polynome (make-polynome 1 2 3))]}
             @item{@interaction[(polynome 'order)]}         
             @item{@interaction[((polynome 'coefficient) 2)]}}