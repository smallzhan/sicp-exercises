;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 以其中的某个门为例，例如 (inverter inout output)
;;;; add-action 的那个过程运行的结果是一个 (after-delay 的时间和动作。
;;;; 其实就是将一个动作加入到指定时间的 agenda 里面，如果不立即执行，没办法
;;;; 在 agenda 里面加入这个延时执行的动作，实际上这个门是没有任何作用的。

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
