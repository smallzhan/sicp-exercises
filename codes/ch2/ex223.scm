(for-each1 (lambda (x) (newline) (display x))
	  (list 57 321 88))

(define (for-each1 fun items)
  (if (not (null? items))
      (fun (car items)))
  (if (null? items)
      'nil
      (for-each1 fun (cdr items))))
      