;as exercise 1.37 && 1.38 && 1.39
(define (cont-frac-iter n d k)
  ;(define count 1)
  (define (frac-iter count result)
    (if (= count k)
        result
        (frac-iter (+ 1 count) (/ (n (- k count)) (+ (d (- k count)) result)))))
  (frac-iter 1 (/ (n k) (d k))))

;;The most important part is `(- k count)' if subtitute this for `k' ,aha, the
;;`phi' program is ok, but the `e-2' program is awful...awful


(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           1000 999)

(cont-frac-iter (lambda (x) 1.0)
           (lambda (x) 1.0)
           1000)

;; (define (cont-frac n d k)
;;   (let ((count 1))
;;     (if (= count k)
;;         (/ (n k) (d k))
;;         (/ (n (- k 1)

(define (cont-frac n d k count)
  (if (> 1 count)
      (/ (n k) (d k))
      (/ (n (- k count))
         (+ (d (- k count)) (cont-frac n d k (- count 1))))))

(define (cont-frac n d k)
  (define (frac-recur count)
    (if (> 1 count)
        (/ (n k) (d k))
        (/ (n (- k count))
           (+ (d (- k count))
              (frac-recur (- count 1))))))
  (frac-recur (- k 1)))

(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           1000)
;;For function `cont-fanc' i do not very satisfy about it, but thank goodness,
;; it works....
;;Maybe oneday I can rewrite it...

(define (de k)
  (if (= (remainder k 3) 2)
      (/ (+ (* 2 k) 2) 3)
      1))

(cont-frac (lambda (x) 1.0) de 1000 999)
;;there must be something wrong with my program, but i can not find it...
;;and i don not know the so-called Eular's rule about `e-2' either..
;;================================================================
;;now i have found where is wrong, it's the caculate order. In my program
;;the order is just the reverse as the right one, but in the test program
;;as N_i and D_i are all the same, that the result is right, but in the program
;;`e-2' or `tg-cf' the N_i and D_i are different from each other, so .....

;;(- (exp 1) 2)

(define (tan-cf x k)
  (define (n-tan y)
    (if (= y 1)
	x
	(- (square x))))
  (define (d-tan y)
    (- (* 2 y) 1))
  (cont-frac n-tan d-tan k (- k 1)))

(tan-cf  (/ 3.1415926535 4) 1000)
