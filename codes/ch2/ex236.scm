(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (car seqs)
      (cons (accumulate op init
			(map (lambda (sub-seq) (car sub-seq)) seqs))
	    (accumulate-n op init
			  (map (lambda (sub-seq) (cdr sub-seq)) seqs)))))

(define test (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(map (lambda (subseq) (cdr subseq)) test)

(accumulate-n + 0 test)