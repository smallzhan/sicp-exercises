(define (same-parity x . y)
  (if (even? x)
      (cons x (even-parity y))
      (cons x (odd-parity y))))

(define (even-parity list)
  (if (null? list)
      list
      (if (even? (car list))
          (cons (car list) (even-parity (cdr list)))
          (even-parity (cdr list)))))

(define (odd-parity list)
  (if (null? list)
      list
      (if (odd? (car list))
          (cons (car list) (odd-parity (cdr list)))
          (odd-parity (cdr list)))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)
