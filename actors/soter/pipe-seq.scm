;; From SOTER benchmarks (pipe). Adapted to use a fixed number of actors that can be distinguished by the analyzer.
;; Bound: pipe-node bounded by 1
(letrec ((pipe-node (actor "pipe-node" (f next)
                           (message (m)
                                    (send next message (f m))
                                    (become pipe-node f next))))
         (sink-actor (actor "sink" ()
                            (message (m) (terminate))))
         (sink (create sink-actor))
         (N 3)
         (f (lambda (x) (+ x 1)))
         (p1 (create pipe-node f sink))
         (p2 (create pipe-node f p1))
         (head (create pipe-node f p2)))
  (send head message 0))
