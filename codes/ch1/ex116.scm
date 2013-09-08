;; as the exercise 1.16
(define (expi b n)
  (if (= n 0)
      1
      (expi-iter b n)))

(define (expi-iter base count)
  (if (= count 1)
      base
      (if (= (remainder count 2) 0)
	  (expi-iter (* base base) (/ count 2))
	  (* base (expi-iter (* base base) (/ (- count 1) 2))))))
