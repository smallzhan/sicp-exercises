(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (real-part  z) (apply-generic 'real-part z))
(define (imag-part  z) (apply-generic 'imag-part z))
(define (magnitude  z) (apply-generic 'magnitude z))
(define (angle  z) (apply-generic 'angle z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test
(define z (make-from-mag-ang 3 1))
(real-part z)
(imag-part z)
(magnitude z)
(angle z)
