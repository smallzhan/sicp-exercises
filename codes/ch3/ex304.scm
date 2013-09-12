;; The process make-account which integrated with
;; password verification..

(define (make-account base passwd)
  (define timer 0)
  (define (withdraw amount)
    (if (>= base amount)
        (begin (set! base (- base amount))
                                        ;(set! timer (+ 1 timer))
               base)
        (error "Insufficient funds")))
  (define (deposit amount)
    (set! base (+ base amount))
    base)
  (define (dispatch secret act)
    (if (>= timer 7)
        (error "I'll call the cops"))
    (if (not (eq? secret passwd))
                                        ;(if (>= timer 7)
                                        ;    (error "I'll call the cops!!"))
        (begin (set! timer (+ 1 timer))
               (error "Incorrect passwd" timer))
        (cond ((eq? act 'withdraw) withdraw)
              ((eq? act 'deposit) deposit)
              (else (error "Unknown request -- make-account"
                           act)))))
                                        ;  (define (call-the-cops)
                                        ;    (display "I'll call the cops"))

                                        ;  (display  timer)

                                        ; (if (>= timer 7)
                                        ;     (call-the-cops)
  dispatch)

(define acc (make-account 100 'hello))
((acc 'hello 'withdraw) 40)
((acc 'hi 'withdraw) 30)
((acc 'hello 'deposit) 100)
