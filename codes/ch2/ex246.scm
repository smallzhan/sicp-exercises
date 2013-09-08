;; (define (make-vect v1 v2)
;;   (cons v1 v2))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))



(define (op-vect  op v1 v2)
  (make-vect (op (xcor-vect v1) (xcor-vect v2))
	     (op (ycor-vect v1) (ycor-vect v2))))

(define (add-vect v1 v2)
  (op-vect + v1 v2))

(define (sub-vect v1 v2)
  (op-vect - v1 v2))

(define (scale-vect k v)
  (make-vect (* k (xcor-vect v))
	     (* k (ycor-vect v))))

(define v1 (make-vect 1 1))
(define v2 (make-vect 0 3))
(add-vect v1 v2)
(sub-vect v1 v2)
(scale-vect 3 v1)
	      

