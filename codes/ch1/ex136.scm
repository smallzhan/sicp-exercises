;as exercise 1.36
(define (fixed-series f first-guess)
  (define (closed? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (closed? next guess)
	  next
	  (try next))))
  (try first-guess))

(define tolerance 0.00001)

;(fixed-series cos 1.0)

(fixed-series (lambda (x) (/ (log 1000) (log x))) 4.0)