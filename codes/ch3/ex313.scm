;;

(define (make-cycle x)
  (set-cdr! (last-pair x) x))

(define (last-pair x)
  (if (null? (cdr x))
	     x
	     (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))

;; what is z?? is a procedure?? a ring???
;; z denotes to the procedure that making a ring using make-cycle.