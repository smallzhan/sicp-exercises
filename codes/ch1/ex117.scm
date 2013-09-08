;as exercise 1.17
(define (multi a b)
  (if (= b 0)
      a
      (if (= (remainder b 2) 0)
	  (multi (double a) (halve b))
	  (+ b (multi (double a) (halve (- b 1))))))