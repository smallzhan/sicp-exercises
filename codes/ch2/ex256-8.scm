;; exrcise 2.56 & exercise 2.57

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


(define (make-sum1 a1 a2) (list '+ a1 a2))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-sum2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((not (pair? a2)) (list '+ a1 a2))
        (else
         (if (sum? a2)
             (cons '+ (cons a1 (cdr a2)))
             (cons '+ (list a1 a2))))))

(define (make-sum3 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (sum3? exp) (eq? (cadr exp) '+))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (augend1 s)
  (let ((tl (caddr s))
        (tr (cdddr s)))
    (if (or (eq? tl '())
            (eq? tr '()))
        tl
        (cons '+ (cddr s)))))

(define (addend3 exp)
  (car exp))


(define (make-product1 m1 m2) (list '* m1 m2))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-product2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((not (pair? m2)) (list '* m1 m2))
        (else
         (if (product? m2)
             (cons '* (cons m1 (cdr m2)))
             (cons '* (list m1 m2))))))

(define (make-product3 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (list m1 '*  m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (product3? exp) (eq? (cadr exp) '*))

(define (multiplier p) (cadr p))

(define (multiplier3 p) (car p))

(define (multiplicand p) (caddr p))

(define (multiplicand2 p)
  (let ((tl (caddr p))
        (tr (cdddr p)))
    (if (or (eq? tl '())
            (eq? tr '()))
        tl
        (cons '* (cddr p)))))


(define (make-exp1 b e) (list '^ b e))

(define (make-exp b e)
  (cond ((=number? b 1) 1)
        ((=number? b 0) 0)
        ((=number? e 0) 1)
        ((and (number? b) (number? e)) (exp b e))
        (else (list '^ b e))))

(define (exp base expo)
  (if (= expo 1)
      base
      (if (even? expo)
          (square (exp base (/ expo 2)))
          (* exp (square (exp base (/ (- expo 1) 2)))))))

(define (exp? s) (and (pair? s) (eq? (car s) '^)))

(define (base s) (cadr s))

(define (expo s) (caddr s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; deriv
(define (deriv-256 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum (deriv-256 (addend exp) var)
                    (deriv-256 (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv-256 (multiplicand exp) var))
          (make-product (deriv-256 (multiplier exp) var)
                        (multiplicand exp))))
        ((exp? exp)
         (make-product
          (make-product (expo exp)
                        (make-exp (base exp) (- (expo exp) 1)))
          (deriv-256 (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (deriv-257 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum? exp)
         (make-sum2 (deriv-257 (addend exp) var)
                    (deriv-257 (augend1 exp) var)))
        ((product? exp)
         (make-sum2
          (make-product2 (multiplier exp)
                         (deriv-257 (multiplicand2 exp) var))
          (make-product2 (deriv-257 (multiplier exp) var)
                         (multiplicand2 exp))))
        ((exp? exp)
         (make-product
          (make-product (expo exp)
                        (make-exp (base exp) (- (expo exp) 1)))
          (deriv-257 (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (deriv-258a exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        ((sum3? exp)
         (make-sum3 (deriv-258a (addend3 exp) var)
                    (deriv-258a (augend exp) var)))
        ((product3? exp)
         (make-sum3
          (make-product3 (multiplier3 exp)
                         (deriv-258a (multiplicand exp) var))
          (make-product3 (deriv-258a (multiplier3 exp) var)
                         (multiplicand exp))))
        ;; ((exp? exp)
        ;;  (make-product
        ;;   (make-product (expo exp)
        ;;                 (make-exp (base exp) (- (expo exp) 1)))
        ;;   (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test
(deriv-257 '(+ x 3 x x (* x 4)) 'x)

(deriv-256 '(* x y) 'x)

(deriv-256 '(^ x 3) 'x)

(deriv-257 '(* x y (+ x 3)) 'x)

(deriv-258a '((x * y) * (x + 3)) 'x)
