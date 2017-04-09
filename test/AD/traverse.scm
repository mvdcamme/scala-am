(define (tree-traverse-nodes tree order action)
  (define (traverse-preorder tree)
    (cond
      ((null-tree? tree)
       #t)
      (else
        (action tree)
        (traverse-preorder (node-left tree))
        (traverse-preorder (node-right tree)))))
  (define (traverse-inorder tree)
    (cond
      ((null-tree? tree)
       #t)
      (else
        (traverse-inorder (node-left tree))
        (action tree)
        (traverse-inorder (node-right tree)))))
  (define (traverse-postorder tree)
    (cond
      ((null-tree? tree)
       #t)
      (else
        (traverse-postorder (node-left tree))
        (traverse-postorder (node-right tree))
        (action tree))))
  (case order
        ((preorder)
         (traverse-preorder tree))
        ((inorder)
         (traverse-inorder tree))
        ((postorder)
         (traverse-postorder tree))
        (else
          (error "Unknown tree traversal order"))))
