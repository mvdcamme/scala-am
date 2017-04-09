(define test (create-graph eq? #f #t))

(test 'insert-node 'a #f)
(test 'insert-node 'b #f)
(test 'insert-node 'c #f)
(test 'insert-node 'd #f)
(test 'insert-node 'e #f)
(test 'insert-node 'f #f)
(test 'insert-node 'g #f)
(test 'insert-node 'h #f)


(test 'insert-edge 'a 'b 2)
(test 'insert-edge 'a 'g 3)
(test 'insert-edge 'b 'c 7)
(test 'insert-edge 'b 'e 2)
(test 'insert-edge 'c 'd 3)
(test 'insert-edge 'c 'f 3)
(test 'insert-edge 'd 'h 2)
(test 'insert-edge 'e 'g 1)
(test 'insert-edge 'e 'f 2)
(test 'insert-edge 'f 'h 2)
(test 'insert-edge 'g 'h 4)


(test 'displ)

