;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;; 此题有陷阱，在两个序对 weight 相等时，实际上它们也是不等的。
;; 所以，应该把两个都 cons 起来，而不是 merge 里面的只 cons 一个，
;; 下面的代码里面多加了一个判断，此题中没用到。其他情况还是有可能用到的。

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((w1 (weight s1car))
                 (w2 (weight s2car)))
           (cond ((< w1 w2)
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> w1 w2)
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (if (equal? s1car s2car)
                      (cons-stream s1car (merge-weighted (stream-cdr s1) (stream-cdr s2) weight))
                      (cons-stream s1car (cons-stream s2car (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)))))))))))

(define (pair-weight pair)
  (+ (car pair) (cadr pair)))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers
  (cons-stream 1 (add-streams ones integers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; a
(define sa (weighted-pairs integers integers pair-weight))

(stream-ref sa 1)
(stream-ref sa 2)
(stream-ref sa 3)
(stream-ref sa 4)
(stream-ref sa 5)
(stream-ref sa 6)
(stream-ref sa 7)
(stream-ref sa 8)
(stream-ref sa 9)
(stream-ref sa 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; b
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define s-235 (cons-stream 1 (merge (scale-stream s-235 2)
                                    (merge (scale-stream s-235 3)
                                           (scale-stream s-235 5)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (pair-weight-235 pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i)
       (* 3 j)
       (* 5 i j))))

(define sb (weighted-pairs s-235 s-235 pair-weight-235))

(stream-ref sb 1)
(stream-ref sb 2)
(stream-ref sb 3)
(stream-ref sb 4)
(stream-ref sb 5)
(stream-ref sb 6)
(stream-ref sb 7)
(stream-ref sb 8)
(stream-ref sb 9)
(stream-ref sb 10)
