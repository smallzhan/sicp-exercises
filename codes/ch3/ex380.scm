;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (RLC R L C dt)
  (define (RLC-process vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (- (/ 1 C))))
    (define dil (add-streams (scale-stream il (- (/ R L)))
                             (scale-stream vc (/ 1 L))))
    (define (vi-stream v i)
      (cons-stream (cons (stream-car v) (stream-car i))
                   (vi-stream (stream-cdr v) (stream-cdr i))))
    (vi-stream vc il))
  RLC-process)
