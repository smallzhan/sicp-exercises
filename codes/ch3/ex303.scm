;; The process make-account which integrated with
;; password verification..

(define (make-account base passwd)
  (define (withdraw amount)
    (if (>= base amount)
	(begin (set! base (- base amount))
	       base)
	(error "Insufficient funds")))
  (define (deposit amount)
    (set! base (+ base amount))
    base)
  (define (dispatch secret act)
    (if (not (eq? secret passwd))
	(error "Incorrect passwd")
	(cond ((eq? act 'withdraw) withdraw)
	      ((eq? act 'deposit) deposit)
	      (else (error "Unknown request -- make-account"
			   act)))))
  dispatch)

(define acc (make-account 100 'hello))
((acc 'hello 'withdraw) 40)
((acc 'hi 'withdraw) 30)