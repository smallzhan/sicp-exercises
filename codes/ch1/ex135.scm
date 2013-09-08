;as exercise 1.35
(define (fixed-point f first-guess)
  (define (closed? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (closed? next guess)
	  next
	  (try next))))
  (define tolerance 0.0001)
  (try first-guess))


(define (golden first-guess)
  (fixed-point (lambda (y) (+ 1 (/ 1 y)))
		first-guess))

 (golden 1.0)