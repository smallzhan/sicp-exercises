(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (flatmap
		(lambda (i)
		  (map (lambda (j) (list i j))
		       (enumerate-interval 1 (- i 1))))
		(enumerate-interval 1 n)))))

(define (filter predicate seq)
  (cond ((null? seq) seq)
	((predicate (car seq))
	 (cons (car seq) (filter predicate (cdr seq))))
	(else
	 (filter predicate (cdr seq)))))

(define (enumerate-interval beg end)
  (if (> beg end)
      ()
      (cons beg (enumerate-interval (+ beg 1) end))))
(define (flatlist n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

     (define (accumulate op initial sequence)
       (if (null? sequence)
           initial
           (op (car sequence)
               (accumulate op initial (cdr sequence)))))
(flatlist 5)

(prime-sum-pairs 5)
(enumerate-interval 1 4)
