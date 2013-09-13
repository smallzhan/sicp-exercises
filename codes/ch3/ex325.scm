;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local tables

(define (assoc keylist records)
  (let ((orig-keylist keylist))
    (define (assoc-inner keylist records)
      (cond ((null? records) false)
            ((null? keylist)
             (if (pair? records)
                 (let ((temp-res (car records)))
                   (if (= (length temp-res) 1)
                       temp-res
                       (assoc-inner orig-keylist (cdr records))))
                 #f))
            ((equal? (car keylist) (caar records))
             (assoc-inner (cdr keylist) (cons (cdar records) (cdr records))))
            (else (assoc-inner keylist (cdr records)))))
    (assoc-inner keylist records)))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup keylist)
      (let ((last-node
             (assoc keylist (cdr local-table))))
        (if last-node
            (car last-node)
            #f)))


    (define (insert! keylist value)
      (let ((last-node (assoc keylist (cdr local-table))))
        (if last-node
            (set-car! last-node value)
            (set-cdr! local-table
                      (cons (append keylist (list value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 特别注意一个序列的 key 是另一个序列的前缀
(put '(1 2 3) 4)
(get '(1 2 3))
(put '(1 2 3 4) 5)
(get '(1 2 3 4))
(put '(1 2 3 4 5) 6)
(put '(1 2 3) 'a)
(put '(1 2 3 4) 'b)
(get '(1 2 3))
(get '(1 2 3 4))
(get '(1 2 3 4 5))
