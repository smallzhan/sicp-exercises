;;exercise 2.2

(define (make-segment start end)
  (cons start end))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (car s))
                    (x-point (cdr s)))
                 2)
              (/ (+ (y-point (car s))
                    (y-point (cdr s)))
                 2)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test 2.02
(define p1 (make-point 1.5 2.5))
(define p2 (make-point 3.5 4.5))
(print-point p1)
(define s (make-segment p1 p2))
(midpoint-segment s)
(print-point (midpoint-segment s))

;;exercise 2.03
(define (make-rect s1 s2)
  (if (or (= (x-point s1) (x-point s2))
           (= (y-point s1) (y-poing s2)))
      (error "Can't make a rectangle for the given two points")
      (cons s1 s2)))

(define (get-left r)
  (car r))

(define (get-right r)
  (cdr r))

(define (rect-length-x  r)
  (abs (- (x-point (get-left r))
          (x-point (get-right r)))))

(define (rect-length-y r)
  (abs (- (y-point (get-left r))
          (y-point (get-right r)))))


(define (rect-area r)
  (* (rect-length-x r)
     (rect-length-y r)))

(define (rect-peri r)
  (* 2
     (+ (rect-length-x r)
        (rect-length-y r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test make-rect

(define rp1 (make-point 1 2))
(define rp2 (make-point 3 4))
(define rect1 (make-rect rp1 rp2))
(rect-area rect1)
(rect-peri rect1)

(define rp3 (make-point 1 3))
(make-rect rp1 rp3) ;;error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;exercise 2.04
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (my-cons x y)
  (define (my-dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Augument not 0 or 1 -- CONS" m))))
  my-dispatch)

(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

(define x (my-cons 3 5))


(define (my-cons2 x y)
  (lambda (m) (m x y)))
(define (my-car2 z)
  (z (lambda (p q) p)))

(define (my-cdr2 z)
  (z (lambda (p q) q)))
(my-cons2 3 5)
(my-car2 (my-cons2 3 5))
(my-cdr2 (my-cons2 3 5))
((lambda (p q) p) 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; exercise 2.05
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (my-cons3 x y)
  (* (expt 2 x) (expt 3 y)))

(define (my-car3 z)
  (define (mod2 x res)
    (if (not (= 0 (remainder x 2)))
        res
        (mod2 (/ x 2) (+ 1 res))))
  (mod2 z 0))

(define (my-cdr3 z)
  (define (mod3 x res)
    (if (not (= 0 (remainder x 3)))
        res
        (mod3 (/ x 3) (+ 1 res))))
  (mod3 z 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test for ex205
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define test-1 (my-cons3 4 5))
(my-car3 test-1)
(my-cdr3 test-1)
