;;; it is very easy to define such an process
;;; because scheme evaluate the expression from left to right
;;; so we define two test function one for (+ (f 0) (f 1)) and
;;; the other (+ (f 1) (f 0))...

(define test
  (let ((temp 1))
    (define (f x)
      (set! temp (- temp 1))
      (* x (+ temp 1)))
    (+ (f 0) (f 1))))

(define test2
  (let ((temp 1))
    (define (f x)
      (set! temp (- temp 1))
      (* x (+ temp 1)))
    (+ (f 1) (f 0))))
