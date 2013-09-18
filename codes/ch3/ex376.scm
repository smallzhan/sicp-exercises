;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (smooth input-stream)
  (let ((s0 (stream-ref input-stream 0))
        (s1 (stream-ref input-stream 1)))
    (cons-stream (/ (+ s0 s1) 2)
                 (smooth input-stream))))

(define smoothed-sense-data (smooth sense-data))

(define zero-crossings (make-zero-crossings smoothed-sense-data 0))
