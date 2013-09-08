;;exercise 2.01

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))


;;;SECTION 2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

                                        ;: (define x (cons 1 2))
                                        ;:
                                        ;: (car x)
                                        ;: (cdr x)

                                        ;: (define x (cons 1 2))
                                        ;: (define y (cons 3 4))
                                        ;: (define z (cons x y))
                                        ;: (car (car z))
                                        ;: (car (cdr z))

;(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

;;footnote -- alternative definitions
(define make-rat cons)
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


                                        ;: (define one-half (make-rat 1 2))
                                        ;:
                                        ;: (print-rat one-half)
                                        ;:
                                        ;: (define one-third (make-rat 1 3))
                                        ;:
                                        ;: (print-rat (add-rat one-half one-third))
                                        ;: (print-rat (mul-rat one-half one-third))
                                        ;: (print-rat (add-rat one-third one-third))


;; reducing to lowest terms in constructor
;; (uses gcd from 1.2.5 -- see ch2support.scm)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


 (define (make-rat-new n d)
   (let ((g (gcd n d)))
     (if (< (* n d) 0)
         (cons (- 0 (abs (/ n g))) (abs (/ d g)))
         (cons (abs (/ n g)) (abs (/ d g))))
     ))




(define (make-rat n d)
  (cons n d))

;; (define (numer x)
;;   (let ((g (gcd (car x) (cdr x))))
;;     (/ (car x) g)))

;; (define (denom x)
;;   (let ((g (gcd (car x) (cdr x))))
;;     (/ (cdr x) g)))


;;;from chapter 1
(define (square x) (* x x))

;;;from section 1.2.5, for Section 2.1.1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; run test
(define neg-third (make-rat-new -1 -3))
(print-rat neg-third)
(numer neg-third)
