;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (make-account balance token)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'token) token)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))
      dispatch)))

(define (serialized-exchange account1 account2)
  (let  ((token1 (account1 'token))
         (token2 (account2 'token)))
    (if (< token1 token2)
        (let (serializer1 (account1 'serializer))
          (let (serializer2 (account2 'serializer))
            ((serializer1 (serializer2 exchange))
             account1
             account2)))
        (let (serialized2 (account2 'serializer))
          (let (serialize1 (account1 'serializer))
            ((serialized2 (serializer1 exchange))
             account2
             account1))))))


(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
