(define (make-segment origin start end)
  (cons (sub-vect start origin)
	(sub-vect end origin)))

(define (start-vect s)
  (car s))

(define (end-vect s)
  (cdr s))

(define (start-segment s)
  (let ((unit (sqrt (+
		     (square (xcor-vect (start-vect s)))
		     (square (ycor-vect (start-vect s)))))))
    (scale-vect (/ 1 unit) start-vect)))

(define (end-segment s)
  (let ((unit (sqrt (+
		     (square (xcor-vect (end-vect s)))
		     (square (ycor-vect (end-vect s)))))))
    (scale-vect (/ 1 unit) end-vect)))
		  