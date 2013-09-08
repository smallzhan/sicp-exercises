;; todo : from exercise 2.7--2.16

(define (percent i)
  (/ (width i) (center i)))

(define (make-center-percent c p)
  (let ((w (* c p)))
    (make-center-width c w)))
