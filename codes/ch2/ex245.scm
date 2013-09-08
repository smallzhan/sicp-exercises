(define (split pos1 pos2)
  (lambda (painter)
    (lambda (n)
      (if (= n 0)
	  painter
	  (let ((half ((split pos1 pos2) painter (- n 1))))
	    (pos1 painter (pos2 half half)))))))

(define right-split (split beside below))
(define up-split (split below beside))