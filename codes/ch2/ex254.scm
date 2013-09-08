

(equal1? '(this is a list) '(this is a list))
(equal1? '(this is a list) '(this is (a) list))

(define (equal1? list1 list2)
  (cond ((null? list1) (null? list2))
	((not (pair? list1)) (and (not (pair? list2)) (eq? list1 list2)))
	(else (and (eq? (car list1) (car list2))
		   (equal1? (cdr list1) (cdr list2))))))

