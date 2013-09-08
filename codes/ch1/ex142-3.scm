;;as exercise 1.42 and 1.43 and 1.44
(define (compose f g)
  (lambda (x)
    (let ((next (g x)))
      (f next))))

(define (inc x)
  (+ x 1))

((compose square inc) 6)

(define (repeated f n)
  (lambda (x)
    (if (= n 2)
	((compose f f) x)
	((compose f (repeated f (- n 1))) x)))) ;; bug n=1...

((compose square square) 3)
((repeated square 2) 5)

(define (smooth f)
  (let ((dx 0.000001))
    (lambda (x)
      (/ (+ (f x) (f (- x dx)) (f (+ x dx)))
	 3.0))))

(define (smooth-n f n)
  (lambda (x)
    ((repeated (smooth f) n) x)))

((smooth-n sin 2) 1)
