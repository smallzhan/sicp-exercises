;;;; 比较直观，没啥可说的，基本就是每次调用生成一个挂在全局环境的新环境。
;;;;
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 实际上，把 fact-iter 定义在 factorial 里面会更有趣，每次调用生成的求值环境
;; 不再挂在全局环境，而是求值 factorial 的那个环境上。

(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))
  (fact-iter 1 1 n))
