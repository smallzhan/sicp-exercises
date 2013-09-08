(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product v w)

(define (matrix-*-vector m v)
  (map (lambda (mat) (dot-product mat v)) m))

(matrix-*-vector m v)

(define (transpose mat)
  (accumulate-n cons () mat))

(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (vec) (matrix-*-vector cols vec)) m)))

(matrix-*-matrix m m)

(define v (list 1 2 3 4))
(define w (list 5 6 7 8))
(define m (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))