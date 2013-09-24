;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; 理论上，只需要修改 判定谓词与其他的东西，这里以 if 为例，例如可以将
;;;; if 修改为 (if xxx then yyy else zzz) 的 6 元组 (好无聊啊), 当然
;;;; 也可以没有 else 后面的，但是不能没有 then
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; (if xxx then yyy else zzz)
;;;; (if xxx then yyy)

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (list-ref exp 1))

(define (if-consequent exp)
  (list-ref exp 3))

(define (if-alternative exp)
  (if (< (length exp) 6)
      'true
      (list-ref exp 5)))
