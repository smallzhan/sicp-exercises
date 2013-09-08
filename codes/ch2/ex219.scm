(define (no-more? list1)
  (null? list1))
      

(define (except-first-denomination list1)
  (if (null? list1)
      list1
      (cdr list1)))

(define (first-denomination list1)
  (if (null? list1)
      0
      (car list1)))

 (define (cc amount coin-values)
            (cond ((= amount 0) 1)
                  ((or (< amount 0) (no-more? coin-values)) 0)
                  (else
                   (+ (cc amount
                          (except-first-denomination coin-values))
                      (cc (- amount
                             (first-denomination coin-values))
                          coin-values)))))

(define us-coins (list 50 25 10 5 1))
us-coins

(cc 100 us-coins)
