(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product count max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
		 (+ count 1)
		 max-count)))

