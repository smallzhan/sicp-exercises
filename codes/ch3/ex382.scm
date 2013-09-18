;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (monto-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ (* passed 1.0) (+ passed failed))
     (monto-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;; (define pi
;;   (stream-map (lambda (p) (sqrt (/ 6 p)))
;;               (monto-carlo cesaro-stream 0 0)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (random-in-range-streams low1 high1 low2 high2)
  (cons-stream (cons (random-in-range low1 high1)
                     (random-in-range low2 high2))
               (random-in-range-streams low1 high1 low2 high2)))

;; (define (gen-exp-stream P s)
;;   (cons-stream
;;    (P (stream-car s))
;;    (gen-exp-stream P (stream-cdr s))))

(define (test-success pair)
  (<= (+ (square (- (car pair) 5))
         (square (- (cdr pair) 7)))
      9))

(define (square x)
  (* x x))

(define (monto-carlo-integral P x1 x2 y1 y2)
  (stream-map (lambda (x) (* x (- x2 x1) (- y2 y1)))
              (monto-carlo (stream-map P (random-in-range-streams x1 x2 y1 y2)) 0 0)))

(define circle-int (monto-carlo-integral test-success 2 8 4 10))
