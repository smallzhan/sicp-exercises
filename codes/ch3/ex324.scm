;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 先做个一维表格，二维应该类似

(define (make-number-table same-key?)
  (let ((local-table (list '*table*)))
    (define (find-key key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            ((same-key? key (caar records)) (car records))
            (else
             (find-key key (cdr records)))))
    (define (lookup key)
      (let ((table (find-key key (cdr local-table))))
        (if table
            (cdr table)
            false)))
    (define (insert! key value)
      (let ((table (find-key key (cdr local-table))))
        (if table
            (set-cdr! table value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- NUMBER-TABLE" m))))
    dispatch))

(define number-table (make-number-table same-key1?))
(define get-number (number-table 'lookup-proc))
(define put-number (number-table 'insert-proc!))

(define (same-key1? key1 key2)
  (if (and (number? key1) (number? key2))
      (< (abs (- key1 key2)) 0.1)
      #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(put-number 3 'a)
(get-number 3)
(get-number 3.1)
(get-number 3.01)
