;; The process make-account which creates an account with passwd verification
;; while the process make-joint shares the account between two accounts

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



(define (make-joint orig-acc orig-secret share-passwd)
  (define (withdraw amount)
    ((orig-acc orig-secret 'withdraw) amount))
  (define (deposit amount)
    ((orig-acc orig-secret 'deposit) amount))
  (define (dispatch secret act)
    (if (not (eq? secret share-passwd))
	(error "Incorrect passwd")
	(cond ((eq? act 'withdraw) withdraw)
	      ((eq? act 'deposit) deposit)
	      (else (error "Unknown request -- make-joint"
			   act )))))
  dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test
(define acc (make-account 100 'hello))
((acc 'hello 'withdraw) 40)
((acc 'hi 'withdraw) 30)
((acc 'hello 'deposit) 100)

(define acc2
  (make-joint acc 'hello 'world))

((acc2 'world 'withdraw) 10)
((acc2 'hello 'deposit) 20)
