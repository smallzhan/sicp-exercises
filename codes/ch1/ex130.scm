;as exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc a)
  (+ a 1))

(define (square a)
  (* a a))

(define (identity a) a)

(sum identity 1 inc 100)
(sum square 1 inc 10)
