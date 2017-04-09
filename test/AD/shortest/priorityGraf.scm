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
