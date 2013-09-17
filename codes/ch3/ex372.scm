;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (weight-square pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i)
       (* j j))))

(define s-372 (weighted-pairs integers integers weight-square))

(define (test-s372 s)
  (let ((pair1 (stream-ref s 0))
        (pair2 (stream-ref s 1))
        (pair3 (stream-ref s 2)))
    (if (and (= (weight-square pair1)
                (weight-square pair2))
             (= (weight-square pair2)
                (weight-square pair3)))
        (cons-stream (list pair1 pair2 pair3 (weight-square pair1))
                     (test-s372 (stream-cdr s)))
        (test-s372 (stream-cdr s)))))

(define s372-series (test-s372 s-372))

(stream-ref s372-series 0)         ;; ((1 18) (6 17) (10 15) 325)
(stream-ref s372-series 1)         ;; ((5 20) (8 19) (13 16) 425)
(stream-ref s372-series 2)         ;; ((5 25) (11 23) (17 19) 650)
(stream-ref s372-series 3)         ;; ((7 26) (10 25) (14 23) 725)
(stream-ref s372-series 4)         ;; ((2 29) (13 26) (19 22) 845)
(stream-ref s372-series 5)         ;; ((3 29) (11 27) (15 25) 850)
