;;

(define (mystery x)
  (define (loop x y)
    (if (null? x)
	y
	(let ((temp (cdr x)))
	  (set-cdr! x y)
	  (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))


;; in fact w is the reverse of v. but it just reverses in the first
;; layer..
;; such as (mystery '(a b (c d))) = ((c d) b a)
