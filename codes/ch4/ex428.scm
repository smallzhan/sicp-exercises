;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; 高阶过程无法应用。

(define (f g x)
  (g x))

;;;; g 会被 delay，这样在求值 (g x) 的时候，g 是个 trunk 而不是实际的函数。