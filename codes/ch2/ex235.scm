(define (accumulate func init seq)
  (if (null? seq)
      init
      (func (car seq)
	    (accumulate func init (cdr seq)))))

(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x)
		     1)
		   (enumerate-tree t))))

(define (enumerate-tree tree)
       (cond ((null? tree) tree)
             ((not (pair? tree)) (list tree))
             (else (append (enumerate-tree (car tree))
                           (enumerate-tree (cdr tree))))))


(define x (list 1 2 (list 3 (list 4 5) 6)))	     

	      
(count-leaves x)
(count-leaves (list x x))