;; Integral of monte-carlo method

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


(define (test P x1 x2 y1 y2)
  (let ((tempx (random-in-range x1 x2))
	(tempy (random-in-range y1 y2)))
    (P tempx tempy)))

(define (monto-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1)
		 (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;(define (int) (integral ))

(define (square n)
  (* n n))

(define (testint x y)
  (<= (+ (square (- x 5))
	 (square (- y 7)))
      9))

;(define (int) (test testint 2 8 4 10))

(define (estimate-integal P x1 x2 y1 y2 trials)
  (define (int) (test P x1 x2 y1 y2))
  (* (monto-carlo trials int)
     (* (- x2 x1) (- y2 y1))))

(estimate-integal testint 2 8 4 10 10000)