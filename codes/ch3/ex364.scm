;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (stream-limit s tolerance)
  (let ((first (stream-car s))
        (second (stream-car (stream-cdr s))))
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr s) tolerance))))


(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x1 x2)
  (/ (+ x1 x2) 2.0))


(sqrt 3 0.001)
