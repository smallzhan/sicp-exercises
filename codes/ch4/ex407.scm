;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (let*->let pairs body)
    (if (null? (cdr pairs))
        (make-let
         (car pairs)
         body)
        (make-let
         (car pairs)
         (let*->let (cdr pairs) body))))
  (let*->let (let*-pairs (cdr exp))
             (let*-body (cdr exp))))

(define (let*-pairs clause) (car clause))
(define (let*-body clause) (cadr clause))

(define (make-let pair body)
  (cons 'let (cons (list pair) (list body))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 不需要派生，直接加入即可。
