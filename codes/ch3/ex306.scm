;; Generate or reset a random series
(define x 0)
(define generate
  ;(let ((x 0))
    (lambda ()
      (set! x (random-update x))
      x))

(define (random-update x)
  (remainder (+ (* 12 x) 11) 17))

(define reset
  (lambda (new)
    (set! x new)
    x))

(define (rand func)
  (cond ((eq? func 'generate) (generate))
	((eq? func 'reset) reset)
	(else
	 (error "Illegal function"))))

(rand 'generate)
((rand 'reset) 0)