(letrec ((lock #f)
         (acq (lambda ()
                (if (cas lock #f #t)
                    #t
                    (acq))))
         (rel (lambda ()
                (set! lock #f)))
         (counter 0)
         (inc (lambda ()
                (acq)
                (set! counter (+ counter 1))
                (rel)))
         (t1 (spawn (inc)))
         (t2 (spawn (inc)))
         (t3 (spawn (inc)))
         (t4 (spawn (inc)))
         (t5 (spawn (inc))))
  (join t1)
  (join t2)
  (join t3)
  (join t4)
  (join t5)
  #t)
