;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;; (define (named-let))

(define (let->combination exp)
  (if (named-let? exp)
      (trans-named-let exp)
      (let ((func (make-lambda
                   (lambda-parameters (let-pairs (let-clause clause)))
                   (let-body (let-clause clause))))
            (exps (let-exps (let-pairs (let-clause clause)))))
        (cons func exps)))


(define (named-let? exp)
  (= (length exp) 4))

(define (named-let-var exp)
  (cadr exp))

(define (named-let-bindings exp)
  (caddr exp))

(define (named-let-body exp)
  (cdddr exp))

(define (trans-named-let exp)
  (let ((funcname (cons (named-let-var exp)
                        (map car (named-let-bindings exp))))
        (funcbody (named-let-body exp)))
    (make-begin
     (list
      (make-define funcname funcbody)
      (cons (named-let-var exp) (map cadr (named-let-bindings exp)))))))

(define (make-begin seq)
  (cons 'begin seq))

(define (make-define funcname body)
  (cons 'define (cons funcname body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; let --> define 变成嵌套 define??? 要不要 begin 框起来？
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test
(define exps '(let fib-iter ((a 1)
                             (b 0)
                             (count n))
                (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))))

(trans-named-let exps)


(begin
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 10))
