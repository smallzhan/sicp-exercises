;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; a. 阶乘程序或 fibonacci 就可以了。没有记忆肯定会慢很多。

(define count 0)

(define (square x)
  (* x x))

(define (id x)
  (set! count (+ count 1)))

(square (id 10)) ;; 100
count ;; 带记忆的版本应该是 1, 不带记忆的版本会是 2， square 中 x 被 delay 了，放到 (* x x) 中，
;; 第一次求值后如果有记忆，则用记忆的值，那么 count 是 1, 否则要求值两次。
