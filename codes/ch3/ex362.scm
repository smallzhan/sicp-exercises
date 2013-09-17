(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "The series has a zero constant...DIV-SERIES")
      (let ((factor (stream-car s2)))
        (scale-stream (mul-series s1 (inverse-series (scale-stream s2 (/ 1 factor)))) factor))))

(define tan-series (div-series sine-series cosine-series))
