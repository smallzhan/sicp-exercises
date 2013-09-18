;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (random-update x)
  (remainder (+ (* 12 x) 11) 17))

(define (handle-command num cmd)
  (cond ((null? cmd) the-empty-stream)
        ((eq? cmd 'generate)
         (random-update num))
        ((and (pair? cmd)
             (eq? (car cmd) 'reset))
         (cdr cmd))
        (else
         error "no such command " cmd)))




(define (random-number-generator command-stream)
  (define random-number
    (cons-stream random-init
                 (stream-map handle-command random-number command-stream)))
  random-number)
