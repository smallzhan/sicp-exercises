;;as exercise 1.29
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
	  dx))

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (inters f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x h))
  (define (add-hh x) (+ x (* 2 h)))
  (+ (/ (* (+ (sum f (+ a h) add-h (- b h))
	      (sum f (+ a h) add-hh (- b h)))
	   h)
	(/ 3 2))
     (/ (* (+ (f a) (f b)) h) 3)))
   ;  (/ (* (f b) dx) 3)))

(define (inter f a b n)
  (define h (/ (- b a) n))
  (define (add-h x) (+ x h))
  (- (* (sum f a add-h b)
	h)
     (* (+ (f a) (f b))
	(/ h 2))))
     
(integral exp 0 1 0.1)
(inter cube 0 1.0 10000)
(inters exp 0 1.0 8)

;; it is strange that the simpon rule is worse, but as n increases ,the result
;; is better and better! Maybe there is something interesting in it...
