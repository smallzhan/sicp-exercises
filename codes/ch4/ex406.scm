;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (let->combination exp)
  (let ((func (make-lambda
               (let-parameters (let-pairs exp))
               (let-body exp)))
        (exps (let-exps (let-pairs exp))))
    (cons func exps)))

(define (let-var pair)
  (car pair))

(define (let-exp pair)
  (cadr pair))

(define (eval-let exp env)
  (eval (let->combination exp)) env)


(define (let? exp)
  (tagged-list? exp 'let))

(define (let-clause exp)
  (cdr exp))

(define (let-pairs exp)
  (cadr exp))

(define (let-body exp)
  (cddr exp))



;; (define (let-parameters pairs)
;;   (define (combine pairs paras)
;;     (if (null? pairs)
;;         paras
;;         (combine (cdr pairs)
;;                  (append paras (list (let-var (car pairs)))))))
;;   (combine pairs '()))

;; (define (let-exps pairs)
;;   (define (combine pairs exps)
;;     (if (null? pairs)
;;         exps
;;         (combine (cdr pairs)
;;                  (append paras (list (let-exp (car pairs)))))))
;;   (combine pairs '()))

(define (let-parameters pairs)
  (map car pairs))

(define (let-exps pairs)
  (map cadr pairs))
