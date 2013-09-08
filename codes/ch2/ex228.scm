(define (fring t)
  (cond ((null? t)
	 t)
	((pair? (car t))
	 (append (fring (car t)) (fring (cdr t))))
      (else (cons (car t) (fring (cdr t))))))

(define x (list (list 1 2) (list 3 4)))
(fring test)
(fring (list x x))
(define test (list x 5 6))

(define (fring-map t)
  (map (lambda (subtree) (append (car subtree) (cdr subtree))) t))

(fring-map x)

x
   
      