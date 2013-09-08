;as exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (identity a) a)

(define (square a)
  (* a a))

(define (inc a)
  (+ a 1))

(define (factorial b)
  (product identity 1 inc b))

;; (factorial 6)	
(define (pi-f x)
  (- 1 (/ 1.0 (square x))))

(define (pi-inc a)
  (+ a 2))

(define (pi-qu a b)
  (product pi-f a pi-inc b))

;; (* 4 (pi-qu 3 50000))
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (pi-qu-iter a b)
  (product-iter pi-f a pi-inc b))

(* 4 (pi-qu-iter 3 50000))