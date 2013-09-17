;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

                                        ;: (display-stream (euler-transform pi-stream))


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s)
                            (stream-cdr s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; 采用下面的 stream-limit-and-display 很容易看出来收敛的速度
;;;;;;;;;;;;;;;;
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define tolerance 0.001)
(stream-limit-and-display ln2-stream tolerance)

(stream-limit-and-display (euler-transform ln2-stream) tolerance)

(stream-limit-and-display (accelerated-sequence euler-transform ln2-stream) tolerance)


(define (stream-limit-and-display s tolerance)
  (display (stream-car s))
  (newline)
  (let ((first (stream-car s))
        (second (stream-car (stream-cdr s))))
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit-and-display (stream-cdr s) tolerance))))
