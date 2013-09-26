;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; transfer to the form in textbook
(define (letrec? exp)
  (tagged-list? exp 'letrec))

(define (letrec-paras exp)
  (cadr exp))

(define (letrec-body exp)
  (cddr exp))

(define (letrec-vars paras)
  (map car paras))

(define (letrec-vals paras)
  (map cadr paras))

(define (vars-unassigned paras)
  (define (var-to-unassigned pair)
    (cons (car pair) (list ''*unassigned*)))
  (map var-to-unassigned paras))

(define (vars-set paras)
  (define (set-var-value pair)
    (cons 'set! pair))
  (map set-var-value paras))

(define (letrec->lets exp)
  (let ((paras (letrec-paras exp))
        (body (letrec-body exp)))
    (cons 'let
          (cons (vars-unassigned paras)
                (append (vars-set paras) body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test
(define test-exp '(letrec ((fact
                            (lambda (n)
                              (if (= n 1)
                                  1
                                  (* n (fact (- n 1)))))))
                    (fact 10)))

(define let-trans (letrec->lets test-exp))
let-trans

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
b.
