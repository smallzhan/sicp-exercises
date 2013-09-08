(define (fold-left op init seq)
  (define (iter res rest)
    (if (null? rest)
	res
	(iter (op res (car rest))
	      (cdr rest))))
  (iter init seq))

(accumulate / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(accumulate list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

(accumulate cons '() (list 1 2 3))
(accumulate cons '() (list 1 2 3))

;; 对于 cons 这样的，会产生一样的结果，也就是说，对计算没有结合性质(比如是左结合
;; 还是右结合这样的没有分别的，就会产生相同的结果.
