(define (inverse-series s)
  (cons-stream 1 (stream-map
                  (lambda (x) (- x))
                  (mul-series (stream-cdr s) (find-recip-series s)))))
