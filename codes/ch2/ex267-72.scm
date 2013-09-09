;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (memeq? key set)
  (if (null? set)
      false
      (if (eq? key (car set))
          true
          (memeq? key (cdr set)))))

(define (encode-symbol symbol tree)
  (define (encode-res tree result)
    ;;(display tree)
    ;;(newline)
    (if (leaf? tree)
        result
        (let ((left (left-branch tree))
              (right (right-branch tree)))
          (if (leaf? left)
              (if (eq? symbol (symbol-leaf left))
                  (append result (list 0))
                  (encode-res right (append result (list 1))))
              (if (memeq? symbol (symbols left))
                  (encode-res left (append result (list 0)))
                  (encode-res right (append result (list 1))))))))
  (encode-res tree '()))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test

(if (equal? (encode (decode sample-message sample-tree) sample-tree)
            sample-message)
    (display "encode right")
    (display "encode wrong"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge leaf-sets)
  ;(display leaf-sets)
  ;(newline)
  (if (< (length leaf-sets) 2)
      (car leaf-sets)
      (let ((min1 (car leaf-sets))
            (min2 (cadr leaf-sets))
            (remain (cddr leaf-sets)))
        (successive-merge (adjoin-set (make-code-tree min1 min2) remain)))))


(generate-huffman-tree '((A 1) (B 2)))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))

(define sample-pair '((A 4) (B 2) (C 1) (D 1)))
(define sample-leafset (make-leaf-set sample-pair))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex.270
(define pair-270 '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))
(define tree-270 (generate-huffman-tree pair-270))

(define msg-270 '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))

(decode (encode msg-270 tree-270) tree-270)
(encode msg-270 tree-270)
(encode '(GET A JOB) tree-270)
(length (encode msg-270 tree-270))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex.2.71
;; 最小的一定是1
;; 最大的是 n-1 (n >= 2) 否则是 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; ex.2.72
;; 编码最频繁的符号，需要 1 位。计算步数为，判断非空一次，car 和 cadr 名一次，eq 一次， null? 一次，
;; 与树的大小无关。因此是 O(1)
;; 编码最不频繁的符号，需要 n-1 位。计算步数：
;; n-2 次调用 encode-res 都遇到 1, 最后一次调用 encode-res 遇到 0, 遇到 0 那次一共是常数次。
;; 第一次遇到 1 需要检查也是常数次，因此是 O(n)
;; 事实上，最麻烦的地方在于需要用到 (memeq? ) 过程的那个地方，不过由于题目限制了相对频度，因此本题中的 huffman
;; 树的编码和解码实际上不会到这个步骤去。
