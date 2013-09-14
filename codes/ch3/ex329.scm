;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; a or b = not ((not a) and (not b))
;;;;;;;;;;;;;;;; delay = 2 * inverter-delay + and-delay

(define (or-gate a b output)
  (let ((na (make-wire))
        (nb (make-ware))
        (no (make-ware)))
    (inverter-gate a na)
    (inverter-gate b nb)
    (and-gate na nb no)
    (invert-gate no output)))
