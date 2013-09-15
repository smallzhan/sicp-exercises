;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 考虑一维表格的2叉树表示
;;;;;;;;;;;;;;;; 没有考虑平衡2叉树的高效实现，使用的最简单的方式。
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-tree x tree)
  (if (null? tree)
      (make-tree x '() '())
      (let ((entry-node (entry tree)))
        (cond
         ((node-eq? x entry-node) tree)
         ((node-le? x entry-node)
          (make-tree entry-node
                    (adjoin-tree x (left-branch tree))
                    (right-branch tree)))
        ((node-ge? x entry-node)
         (make-tree entry-node
                    (left-branch tree)
                    (adjoin-tree x (right-branch tree))))))))

(define (node-eq? node1 node2)
  (= (key-of-node node1) (key-of-node node2)))

(define (node-le? node1 node2)
  (< (key-of-node node1) (key-of-node node2)))

(define (node-ge? node1 node2)
  (> (key-of-node node1) (key-of-node node2)))


(define (key-of-node node)
  (car node))

(define (value-of-node node)
  (cdr node))


(define (lookup-tree key tree)
  (if (null? tree)
      #f
      (let ((node (entry tree)))
        (cond ((= key (key-of-node node)) node)
              ((> key (key-of-node node))
               (lookup-tree key (right-branch tree)))
              ((< key (key-of-node node))
               (lookup-tree key (left-branch tree)))))))


(define (make-tree-table)
  (let ((local-tree '()))
    (define (lookup key)
      (let ((res (lookup-tree key local-tree)))
        (if res
            (value-of-node res)
            #f)))
    (define (insert! key value)
      (let ((res (lookup-tree key local-tree)))
        (if res
            (set-cdr! res value)
            (begin
              (set! local-tree (adjoin-tree (cons key value) local-tree))
              (set! local-tree (list->tree (tree->list local-tree)))
              )))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print) (lambda () (display local-tree)
                                    (newline)))
            (else (error "Unknown operation -- TREE-TABLE" m))))
    dispatch))


(define tree-table (make-tree-table))
(define get-tree-table (tree-table 'lookup-proc))
(define put-tree-table (tree-table 'insert-proc!))
