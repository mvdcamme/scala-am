(let* ((size 128)
       (max 4)
       (table (make-vector size 0))
       (thread (lambda (tid)
                 (letrec ((hash (lambda (w) (modulo (* w 7) size)))
                          (process (lambda (m)
                                     (if (< m max)
                                         (letrec ((w (+ (* 11 (+ m 1)) tid))
                                                  (update (lambda (h)
                                                            (if (cas-vector table h 0 w)
                                                                #t
                                                                (update (modulo (+ h 1) size))))))
                                           (update (hash w))
                                           (process (+ m 1)))
                                         #t))))
                   (process 0))))
(t1 (spawn (thread 1)))
(t2 (spawn (thread 2)))
(t3 (spawn (thread 3)))
(t4 (spawn (thread 4)))
(t5 (spawn (thread 5)))
(t6 (spawn (thread 6)))
(t7 (spawn (thread 7)))
(t8 (spawn (thread 8)))
(t9 (spawn (thread 9)))
(t10 (spawn (thread 10)))
(t11 (spawn (thread 11)))
(t12 (spawn (thread 12)))
(t13 (spawn (thread 13))))
(join t1)
(join t2)
(join t3)
(join t4)
(join t5)
(join t6)
(join t7)
(join t8)
(join t9)
(join t10)
(join t11)
(join t12)
(join t13))