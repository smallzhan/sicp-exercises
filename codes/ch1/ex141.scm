;;as exercise 1.41
(define (double g)
  (lambda (x)
    (let ((once (g x)))
      (g once))))

(define (inc x)
  (+ x 1))


(((double (double double)) inc) 5)