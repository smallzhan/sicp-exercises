;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; it looks like this.
(define (solve-2nd a b dt y0 dy0 dy/dt)
  (define y (integral dy y0 dt))
  (define dy (integral ddy dy0 dy/dt))
  (define ddy (add-streams
               (scale-stream  dy a)
               (scale-stream y b)))
  y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; now delay dy and ddy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; the right version
(define (solve-2nd a b dt y0 dy0 dy/dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dy/dt))
  (define ddy (add-streams
               (scale-stream dy a)
               (scale-stream y b)))
  y)
