;as exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity a) a)

(define (square a)
  (* a a))

(define (inc a)
  (+ a 1))

;; (sum identity 1 inc 100)
;; (product identity 1 inc 10)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter combiner a result)
    (if (> a b)
	result
	(iter combiner (next a) (combiner (term a) result))))
  (iter combiner a null-value))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))


;; (sum-iter identity 1 inc 100)


