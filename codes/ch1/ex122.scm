;as exercise 1.22
(define (time-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (not (prime? n))
       (display " not prime")
       (report-prime-time (- (runtime) start-time))))

(define (report-prime-time elasped-time)
  (display " *** ")
  (display elasped-time))

;; (time-prime-test 1999999999)
;; (report-prime-time 1000)
;; (prime? 5374052401391)

(define (search-for-prime base times)
  ;(eval-time (runtime))
  (cond ((= times 0))
	((even? base) (search-for-prime (+ base 1) times))
	((prime? base) ((search-for-prime (+ base 2) (- times 1))
			(display base)
			(display " *** ")))
	(else (search-for-prime (+ base 2) times)))
  (trace-both search-for-prime)
  )

(define (eval-time base)
  (define start-time (runtime))
  (search-for-prime base 3)
  (report-prime-time (- (runtime) start-time)))

(define (start-search-prime base)
  (newline)
  (eval-time base)
  (trace-both eval-time)
  (trace-both search-for-prime))

(define (success)
  (display " time "))
 
;; (start-search-prime 1000000)
;; (time-prime-test 1000033)
;; (prime? 10000033)