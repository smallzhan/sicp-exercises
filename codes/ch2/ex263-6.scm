;; exercise 2.63 -- exercise 2.65

;; BINARY TREES
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; EXERCISE 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex 2.63
(define tree-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 tree-1)
(tree->list-1 tree-2)
(tree->list-1 tree-3)

(tree->list-2 tree-1)
(tree->list-2 tree-2)
(tree->list-2 tree-3)

;; tree->list-1 是递归， tree->list-2 是迭代版本。 迭代版本明显增长慢一些，具有较好的效率。
;; tree->list-2 应该是线性， tree->list-1 额，是第一章讲的经典的树形递归。

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex 2.64
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

(list->tree '(1 3 5 7 9 11))
;; '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;; 很简单，递归， 将数据最中间那个拿出来作为根，然后左边形成左子，右边形成右子。
;; 每次从中间递归拿，然后再 cons 起来。设总共是 T(n), 那么
;; T(n) = 1 + T((n-1) / 2) + T(n - (n-1) / 2 - 1) 类似，递归解出来复杂度是线性的 O(n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex2.65

(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
		(list2 (tree->list-2 set2)))
	(let ((list12 (union-set-list list1 list2)))
	  (list->tree list12))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
		(list2 (tree->list-2 set2)))
	(let ((list12 (intersection-set-list list1 list2)))
	  (list->tree list12))))


(define (union-set-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set-list (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set-list (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set-list set1 (cdr set2)))))))))

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-list
						 (cdr set1)
						 (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-list set1 (cdr set2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tree-1 (list->tree '(1 2 3 4 5)))
(define tree-2 (list->tree '(2 3 4 6 7 8)))

(union-set tree-1 tree-2)
(intersection-set tree-1 tree-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 有点耍赖，不过这个应该是题目的要求吧。。。
;;;;;;;;;;;;;;;; 就是利用前面两个的题目的结果，不然树的合并相当麻烦。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (lookup-tree given-key tree)
  (if (null? tree)
      false
      (let ((x (entry tree)))
        (cond ((= given-key x) x)
              ((> given-key x) (lookup-tree given-key (right-branch tree)))
              ((< given-key x) (lookup-tree given-key (left-branch tree)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test ex 2.66
(lookup-tree 3 tree-1)
(lookup-tree 9 tree-1)
