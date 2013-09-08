;; make a accumulator

(define (make-accumulator sum)
  (lambda (acc)
    (begin (set! sum (+ sum acc))
	   sum)))

(define A (make-accumulator 5))

(A 10)