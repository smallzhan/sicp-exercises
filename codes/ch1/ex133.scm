;as exercise 1.33
(define (filtered-accumulate filter
			      combiner
			      null-value
			      term
			      a
			      next
			      b)
  (cond ((> a b) null-value)
	((filter (term a))
	 (combiner (term a) (filtered-accumulate filter
						 combiner
						 null-value
						 term
						 (next a)
						 next
						 b)))
	(else (filtered-accumulate filter
				   combiner
				   null-value
				   term
				   (next a)
				   next
				   b))))

(filtered-accumulate even? + 0 identity 1 inc 100)

(define (inc n) (+ n 1))
