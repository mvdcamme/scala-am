(define true #t)

(define false #f)

(define (make-item priority element)
  (cons priority element))

(define (get-priority item)
  (car item))

(define (get-element item)
  (cdr item))

(define (create-priority-queue)
  (let ((front (cons 'boe '())))
    (define (content) (cdr front))
    (define (insert-after! cell item)
      (let ((new-cell (cons item '())))
        (set-cdr! new-cell (cdr cell))
        (set-cdr! cell new-cell)))
    (define (find-prev-cell priority)
      (define (find-iter rest prev)
        (cond
          ((null? rest) prev)
          ((> (get-priority (car rest)) priority)
           (find-iter (cdr rest) rest))
          (else prev)))
      (find-iter (content) front))
    (define (empty?)
      (null? (content)))
    (define (enqueue priority element)
      (insert-after! (find-prev-cell priority)
                     (make-item priority element))
      true)
    (define (dequeue)
      (if (null? (content))
          false
          (let ((temp (car (content))))
            (set-cdr! front (cdr (content)))
            (get-element temp))))
    (define (serve)
      (if (null? (content))
          false
          (get-element (car (content)))))
    (define (dispatch m)
      (cond
        ((eq? m 'empty?) empty?)
        ((eq? m 'enqueue) enqueue)
        ((eq? m 'dequeue) dequeue)
        ((eq? m 'serve) serve)
        (else
         (error "unknown request --
 create-priority-queue" m))))
    dispatch))

(define (create-graph eq-label-ft directed? weighted?)
  (let ((nodes '()))
    (define (make-edge node1 node2 info)
      (if weighted?
          (list node1 node2 '() (car info))
          (list node1 node2 '() 'notused)))
    (define (get-first-node edge)
      (car edge))
    (define (get-second-node edge)
      (cadr edge))
    (define (get-next-edge edge)
      (caddr edge))
    (define (set-next-edge! edge next-edge)
      (set-car! (cddr edge) next-edge))
    (define (get-edge-info edge)
      (cadddr edge))
    (define (set-edge-info! edge info)
      (set-car! (cdddr edge) info))
    (define (make-node label info status)
      (list label info '() (if (null? status) 'not-used (car status))))
    (define (get-label node)
      (car node))
    (define (set-label! node label)
      (set-car! node label))
    (define (get-node-info node)
      (cadr node))
    (define (set-node-info! node a-info)
      (set-car! (cdr node) a-info))
    (define (get-node-status node)
      (cadddr node))
    (define (set-node-status! node status)
      (set-car! (cdddr node) status))
    (define (get-edges node)
      (caddr node))
    (define (set-edges! node edges)
      (set-car! (cddr node) edges))
    (define (add-to-edges node edge)
      (set-next-edge! edge (get-edges node))
      (set-edges! node edge))
    (define (find-node label)
      (define (find-iter current)
        (cond
          ((null? current) #f)
          ((eq-label-ft label (get-label (car current))) (car current))
          (else (find-iter (cdr current)))))
      (find-iter nodes))
    (define (find-edge label1 label2)
      (define (find-iter current node)
        (cond
          ((null? current) #f)
          ((eq? node (get-second-node current)) current)
          (else (find-iter (get-next-edge current) node))))
      (let ((node1 (find-node label1))
            (node2 (find-node label2)))
        (if directed?
            (if (and node1 node2)
                (find-iter (get-edges node1) node2)
                #f)
            (if (and node1 node2)
                (cons (find-iter (get-edges node1) node2)
                      (find-iter (get-edges node2) node1))
                (cons #f #f)))))
    (define (insert-node label info)
      (let ((node (find-node label)))
        (cond
          (node (set-node-info! node info))
          (else (set! nodes (cons (make-node label info) nodes))
                #t))))
    (define (insert-edge label1 label2 info)
      (let ((edge (find-edge label1 label2)))
        (cond
          ((and directed? edge)
           (if weighted? (set-edge-info! edge (car info)))
           #t)
          ((and (not directed?) (car edge) (cdr edge))
           (if weighted?
               (begin (set-edge-info! (car edge) (car info))
                      (set-edge-info! (cdr edge) (car info))))
           #t)
          (else
            (let* ((node1 (find-node label1))
                   (node2 (find-node label2)))
              (if (and node1 node2)
                  (let ((edge (if weighted?
                                  (make-edge node1 node2 (car info))
                                  (make-edge node1 node2))))
                    (if directed?
                        (add-to-edges node1 edge)
                        (let ((edge-dupl (if weighted?
                                             (make-edge node2 node1 (car info))
                                             (make-edge node2 node1))))
                          (add-to-edges node1 edge)
                          (add-to-edges node2 edge-dupl)))
                    #t)
                  #f))))))
    (define (delete-edge label1 label2)
      (define (delete-iter current previous node1 node2)
        (cond
          ((null? current) #f)
          ((eq? (get-second-node current) node2)
           (if previous
               (set-next-edge! previous (get-next-edge current))
               (set-edges! node1 (get-next-edge current)))
           #t)
          (else
            (delete-iter (get-next-edge current) current node1 node2))))
      (let ((node1 (find-node label1))
            (node2 (find-node label2)))
        (if (and node1 node2)
            (if directed?
                (delete-iter (get-edges node1) #f node1 node2)
                (and
                  (delete-iter (get-edges node1) #f node1 node2)
                  (delete-iter (get-edges node2) #f node2 node1)))
            #f)))
    (define (delete-node label)
      (define (delete-iter current prev)
        (cond
          ((null? current) #f)
          ((eq-label-ft label (get-label (car current)))
           (if prev
               (set-cdr! prev (cdr current))
               (set! nodes (cdr nodes)))
           #t)
          (else (delete-iter (cdr current) current))))
      (delete-iter nodes #f))
    (define (lookup-node-info label)
      (let ((node (find-node label)))
        (if node
            (get-node-info node)
            #f)))
    (define (change-node-info label info)
      (let ((node (find-node label)))
        (if node
            (set-node-info! node info)
            #f)))
    (define (lookup-node-status label)
      (let ((node (find-node label)))
        (if node
            (get-node-status node)
            #f)))
    (define (change-node-status label status)
      (let ((node (find-node label)))
        (if node
            (set-node-status! node status)
            #f)))
    (define (lookup-edge label1 label2)
      (let ((edge (find-edge label1 label2)))
        (if directed?
            (if edge
                (if weighted? (get-edge-info edge) #t)
                #f)
            (if (and (car edge) (cdr edge))
                (if weighted? (get-edge-info (car edge)) #t)
                #f))))
    (define (empty?)
      (null? nodes))
    (define (map-over-nodes a-function)
      (define (map-iter current result)
        (if (null? current)
             (reverse result)
            (map-iter (cdr current)
                      (cons (a-function (get-label (car current))
                                        (get-node-info (car current)))
                            result))))
      (map-iter nodes '()))
    (define (foreach-node a-action)
      (define (foreach-iter current)
        (cond
          ((null? current) #t)
          (else
            (a-action (get-label (car current)) (get-node-info (car current)))
            (foreach-iter (cdr current)))))
      (foreach-iter nodes))
    (define (map-over-neighbours label a-function)
      (let ((node (find-node label)))
        (define (edges-iter current result)
          (if (null? current)
              result
              (let* ((neigbour-node (get-second-node current))
                     (res (a-function
                            (get-label node)
                            (get-node-info node)
                            (get-label neigbour-node)
                            (get-node-info neigbour-node)
                            (get-edge-info current))))
                (edges-iter (get-next-edge current) (cons res result)))))
        (if node
            (edges-iter (get-edges node) '())
            #f)))
    (define (foreach-neighbour label a-action)
      (let ((node (find-node label)))
        (define (edges-iter current)
          (if (null? current)
              #t
              (let* ((neigbour-node (get-second-node current)))
                     (a-action
                       (get-label node)
                       (get-node-info node)
                       (get-label neigbour-node)
                       (get-node-info neigbour-node)
                       (get-edge-info current))
                (edges-iter (get-next-edge current)))))
        (if node
            (edges-iter (get-edges node))
            #f)))
    (define (dispatch msg args)
      (cond
        ((eq? msg 'empty?) (empty?))
        ((eq? msg 'insert-node) (apply insert-node args))
        ((eq? msg 'delete-node) (apply delete-node args))
        ((eq? msg 'insert-edge) (apply insert-edge args))
        ((eq? msg 'delete-edge) (apply delete-edge args))
        ((eq? msg 'lookup-node-info) (apply lookup-node-info args))
        ((eq? msg 'change-node-info) (apply change-node-info args))
        ((eq? msg 'lookup-node-status) (apply lookup-node-status args))
        ((eq? msg 'change-node-status) (apply change-node-status args))
        ((eq? msg 'lookup-edge) (apply lookup-edge args))
        ((eq? msg 'map-over-nodes) (apply map-over-nodes args))
        ((eq? msg 'foreach-node) (apply foreach-node args))
        ((eq? msg 'map-over-neighbours) (apply map-over-neighbours args))
        ((eq? msg 'foreach-neighbour) (apply foreach-neighbour args))
        ((eq? msg 'find-node) (apply find-node args))
        ((eq? msg 'displ) (display nodes))
        (else
          (display msg))))
    dispatch))


(define (priority-first-traversal graph
                                  start-label
                                  make-queue-element
                                  priority-ft
                                  eq-queue-element
                                  access-label
                                  action)
  (define (processed? label)
    (eq?  (graph 'lookup-node-status label)
          'P))
  (if (graph 'empty?)
      #f
      (let ((queue (create-priority-queue)))
        (define (iter)
          (cond
            (((queue 'empty?)) 'empty)
            (else (let* ((priority-item ((queue 'dequeue)))
                         (item priority-item))
                    (graph 'change-node-status (access-label item) 'P)
                    (action item)
                    (graph 'foreach-neighbour (access-label item)
                           (lambda (from-label from-info
                                               to-label to-info edge-info)
                             (if (not (processed? to-label))
                                 (begin
                                   (graph 'change-node-status
                                          to-label 'R)
                                   ((queue 'enqueue)
                                    (priority-ft
                                      from-label from-info
                                      to-label to-info edge-info)
                                    (make-queue-element
                                      from-label from-info
                                      to-label to-info edge-info))))))
                    (iter)))))
        (graph 'foreach-node
               (lambda (label info)
                 (graph 'change-node-status label 'W)))

        ((queue 'enqueue) (priority-ft #f #f
                                       start-label
                                       (graph 'lookup-node-info start-label)
                                       #f)
         (make-queue-element #f #f
                             start-label
                             (graph 'lookup-node-info start-label)
                             #f))
        (iter))))

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


(define (compute-shortest-path graph from-label to-label)
  (define (read-out-path current)
    (cond
      ((eq? current from-label)
       (list current))
      (else
        (append
          (read-out-path
            (car (graph 'lookup-node-info current)))
          (list current)))))

  (graph 'map-over-nodes (lambda (x y)
                           (graph 'change-node-info x #f)))
  
  
  (priority-first-traversal
    graph
    from-label
    (lambda (a b c d e)
      (list a (if b (cdr b) 0)
            c (if e e 0)))
    (lambda (a b c d e)
      (if e
          (/ 1 (+ e (cdr b)))
          1))
    (lambda (item1 item2)
      (eq? (caddr item1)
           (caddr item2)))
    (lambda (item) (caddr item))
    (lambda (item)
      (graph 'change-node-info
             (caddr item)
             (cons (car item)
                   (+ (cadr item)
                      (cadddr item))))))
  (read-out-path to-label))

(compute-shortest-path test 'a 'f)