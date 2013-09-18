;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (RC R C dt)
  (lambda (i v0)
    (integral-RC i v0 R C dt)))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


(define (integral-RC i v0 R C dt)
  (define int-RC
    (integral (scale-stream i (/ 1 C) v0 dt))
  (add-streams int-RC
               (stream-scale i R)))

(define RC1 (RC 5 1 0.5))
