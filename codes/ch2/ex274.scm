;; exercise 2.74

;; a. 每个子公司有自己的 get-record，使用两个参数，文件，
(define (get-record file emp)
  (let ((proc (get file 'get-record)))
    (if proc
        (apply (proc emp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 每个子公司自己的操作作为内部过程，put 时加上公司的标签
;;;;;;;;;;;;;;;; 以及通用的操作的tag
