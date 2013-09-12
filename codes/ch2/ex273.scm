;; exercise 2.73
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; put and get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
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
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (variable? x)
  (symbol? x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a.将每一种类型的 deriv 操作安装到了对应类型下。
;; 无法分派的原因是没有对应的“类型标志”

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp))
          (operands exp)
          var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  (define (sum? exp)
    (eq? (operator exp) '+))
  (define (product? exp)
    (eq? (operator exp) '*))
  (define (expm? exp)
    (eq? (operator exp) '^))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (same-variable? var1 var2)
    (and (symbol? var1) (symbol? var2) (eq? var1 var2)))
  ;; (define (addend exp)
  ;;   (cadr exp))
  ;; (define (augend exp)
  ;;   (caddr exp))
  ;; (define (multiplicand exp)
  ;;   (cadr exp))
  ;; (define (multiplier exp)
  ;;   (caddr exp))
  ;; (define (expmbase exp)
  ;;   (cadr exp))
  ;; (define (expmexpo exp)
  ;;   (caddr exp))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (make-exp b e)
    (cond ((=number? b 1) 1)
          ((=number? b 0) 0)
          ((=number? e 0) 1)
          ((and (number? b) (number? e)) (exp b e))
          (else (list '^ b e))))

  (define (deriv-trival arg var)
    (cond ((number? arg) 0)
          ((symbol? arg)
           (if (same-variable? arg var)
               1
               0))))

  (define (deriv-sum args var)
    (let ((arg1 (car args))
          (arg2 (cadr args)))
      ;;(display arg1 arg2)
      (make-sum (if (pair? arg1)
                    (cond ((sum? arg1) (deriv-sum (operands arg1) var))
                          ((product? arg1) (deriv-product (operands arg1) var))
                          ((expm? arg1) (deriv-expm (operands arg1) var)))
                    (deriv-trival arg1 var))
                (if (pair? arg2)
                    (cond ((sum? arg2) (deriv-sum (operands arg2) var))
                          ((product? arg2) (deriv-product (operands arg2) var))
                          ((expm? arg2) (deriv-expm (operands arg2) var)))
                    (deriv-trival arg2 var))
                )))

  (define (deriv-product args var)
    (let ((arg1 (car args))
          (arg2 (cadr args)))
      (make-sum
       (make-product
        arg2
        (if (pair? arg1)
            (cond ((sum? arg1) (deriv-sum (operands arg1) var))
                  ((product? arg1) (deriv-product (operands arg1) var))
                  ((expm? arg1) (deriv-expm (operands arg1) var)))
            (deriv-trival arg1 var)))
       (make-product
        (if (pair? arg2)
            (cond ((sum? arg2) (deriv-sum (operands arg2) var))
                  ((product? arg2) (deriv-product (operands arg2) var))
                  ((expm? arg2) (deriv-expm (operands arg2) var)))
            (deriv-trival arg2 var))
        arg1
        ))))

  (define (deriv-expm args var)
    (let ((arg1 (car args))
          (arg2 (cadr args)))
      (make-product
       (make-product arg2
                     (make-exp arg1 (- arg2 1)))
       (if (pair? arg1)

           (cond ((sum? arg1) (deriv-sum (operands arg1) var))
                 ((product? arg1) (deriv-product (operands arg1) var))
                 ((expm? arg1) (deriv-expm (operands arg1) var)))
           (deriv-trival arg1 var)))
      ))

  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '^ deriv-expm)
  'done)


(deriv '(* 3 x) 'x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 可加性，是说不用管 deriv 的接口，只要在 install 这里直接实现
;;;;;;;;;;;;;;;; 然后 put 进去，就实现了新的 deriv 的接口。

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;d. 这个只要修改 put 部分，将前面两个参数符号反过来就行了。



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;数据导向的递归方式, 实现方式更加简洁
;;;;;;;;;;;;;;;;通过外部过程 deriv2 进行递归，不需要考虑内部具体结构
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (install-deriv2-package)
  (define (=number? x y)
    (and (number? x) (number? y) (= x y)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (make-exp b e)
    (cond ((=number? b 1) 1)
          ((=number? b 0) 0)
          ((=number? e 0) 1)
          ((and (number? b) (number? e)) (exp b e))
          (else (list '^ b e))))

  (define (deriv-sum args var)
    (let ((arg1 (car args))
          (arg2 (cadr args)))
      (display arg1)
      (display "---")
      (display arg2)
      (newline)
      (make-sum (deriv2 arg1 var)
                (deriv2 arg2 var))))

 (define (deriv-product args var)
    (let ((arg1 (car args))
          (arg2 (cadr args)))
      (make-sum
       (make-product
        arg2
        (deriv2 arg1 var))
       (make-product
        arg1
        (deriv2 arg2 var)))))

  (define (deriv-expm args var)
    (let ((arg1 (car args))
          (arg2 (cadr args)))
      (make-product
       (make-product arg2
                     (make-exp arg1 (- arg2 1)))
       (deriv2 arg1 var))))

  (put 'deriv2 '+ deriv-sum)
  (put 'deriv2 '* deriv-product)
  (put 'deriv2 '^ deriv-expm)
  'done)

(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv2 (operator exp))
          (operands exp)
          var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-deriv2-package)

(deriv2 '(* x (^ x 3)) 'x)
(deriv2 '(+ x 3) 'x)
(deriv2 '(+ x (+ x 3)) 'x)
