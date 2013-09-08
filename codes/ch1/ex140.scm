;;as exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (cube x)
  (* x x x))

;(cube 3)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
;; (define (newton-transform g)
;;        (lambda (x)
;;          (- x (/ (g x) ((deriv g) x)))))
(define (deriv g)
  (define dx 0.000001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

;; ((deriv cube) 5)

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.000001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

 
 (define (sqrt x)
       (newtons-method (lambda (y) (- (square y) x))
                       1.0))


;(fixed-point (lambda (x) (- (square x) 2)) (- 2))

 
 (newtons-method (cubic (- 7.7) 19.2 (- 15.3)) 1)

