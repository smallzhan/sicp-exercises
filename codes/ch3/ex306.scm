;; Generate or reset a random series
(define x 0)
(define generate
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; using local variable
(define (rand-local)
  (let ((randvar 0))
    (define generate
      (lambda ()
        (set! randvar (random-update randvar))
        randvar))
    (define (random-update x)
      (remainder (+ (* 12 x) 11) 17))
    (define (reset newval)
      (set! randvar newval))
    (define (dispatch m)
      (cond ((eq? m 'generate) generate)
            ((eq? m 'reset) reset)
            (else
             (error "Illegal function"))))
    dispatch))



(define rand (rand-local))
((rand 'generate))
((rand 'reset) 0)
