;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a <e1>)
          (b <e2>))
      (set! u a)
      (set! v b))
    <e3>))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;;;; u 即定义中 y， v 是 dy， 那么 由于 dy 被 delay 了，所以 <e1> 求到 a 是可以的。
;;;; 但是此时 u 也就是 y 还是 unassigned， 所以求值 <e2> 的时候会遇到 unassigned
;;;; 正文中的没这个问题。
