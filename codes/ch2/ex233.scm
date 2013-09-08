(define (accumulate func initial seq)
  (if (null? seq)
      initial
      (func (car seq)
	   (accumulate func initial (cdr seq)))))

(define (map-accu p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      ()
	      sequence)) 

(define test (list 1 2 3 4))
(map-accu square test)

(define test2 (list 5 6 7 8))

(define (append-accu seq1 seq2)
  (accumulate cons seq2 seq1))

(append-accu test test2)

(define (length sequence)
  (accumulate (lambda (x y)
		    (+ y 1))
	      0 sequence))

(length test2)
