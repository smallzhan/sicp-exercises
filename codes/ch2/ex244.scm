(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split paint (- n 1))))
	(below painter (beside smaller samller)))))