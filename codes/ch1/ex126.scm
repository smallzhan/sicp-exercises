;; execise 1.26
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmpd base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


;; 参见 Applicative order 的求值过程。
;; (square x) 先将 x 计算出来再进行 (* x x) 的工作，如果写成显式的，(expmod ) 那个调用会求解两次。
;; 每一次都会计算两次，因此将 logn 变 n， 将 n 变指数 2^n
