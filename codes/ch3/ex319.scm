;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 只需要常量空间，建立原始串的指针并保存，每次查看。
;; 指导思想与 3.18 一致，某次 cdr 出现以前的 cdr 已经出现的结果即可判断有环。


(define (cycle? x)
  (define orig-x x)
  (define (cycle-set x count)
    (if (not (pair? x))
        #f
        (if (check x orig-x count)
            #t
            (cycle-set  (cdr x) (+ count 1)))))
  (cycle-set x 0))

(define (check x y count)
  (if (< count 1)
      #f
      (if (eq? x y)
          #t
          (check x (cdr y) (- count 1)))))
