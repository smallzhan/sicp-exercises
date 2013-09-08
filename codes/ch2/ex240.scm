(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (if (< j i)
			     (list i j)))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(unique-pairs 6)

(define (prime-sum-pairs-u n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

(prime-sum-pairs-u 6)
				      
	     