(define (eq-tri n)
  ;(map cons
	 (filter eq-s (tri n)))

(define (eq-s trip)
  (= (+ (car trip) (cadr trip) (caddr trip)) 5))

(filter eq-s (tri 3))
(tri 2)
       ;(tri n)))

(define (tri n)
  (flatmap (lambda (i)
	         (flatmap (lambda (j)
			(map (lambda (k)
			      (list i j k))
			     (enumerate-interval 1 n)))
		      (enumerate-interval 1 n)))
	   (enumerate-interval 1 n)))

(define (filter-tri s trip)
  (filter (= (+ (car trip) (cadr trip) (caddr trip))) s)))

(define l (list 1 2 3))

(eq-tri 3)
