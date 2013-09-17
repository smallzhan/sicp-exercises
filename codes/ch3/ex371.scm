;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (weight-cubic pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* i i i)
       (* j j j))))

(define s (weighted-pairs integers integers weight-cubic))

(define (ramanujan s)
  (let ((pair1 (stream-car s))
        (pair2 (stream-car (stream-cdr s))))
    (if (= (weight-cubic pair1)
           (weight-cubic pair2))
        (cons-stream (weight-cubic pair1)
                     (ramanujan (stream-cdr s)))
        (ramanujan (stream-cdr s)))))

(define ramanujan-series (ramanujan s))

(stream-ref ramanujan-series 0)         ;1729
(stream-ref ramanujan-series 1)         ;4104
(stream-ref ramanujan-series 2)         ;13832
(stream-ref ramanujan-series 3)         ;20683
(stream-ref ramanujan-series 4)         ;32832
(stream-ref ramanujan-series 5)         ;39312
