;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (a-pythagorean-triple-all)
  (let ((i (an-integer-starting-from 0)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-starting-from j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 会一直枚举 k 不会回溯，而成为死循环。
;;;; 正解可以仿照 4.37 的方式确定 k， 对于
;;;; (i j) 当 (j+1)^2 - j^2 > i^2 的时候就退出进一步增加 i。
;;;; 由于 i^2 + j^2 < (j+1)^2 因此此时不可能有合适的 k。
;;;; i 不断增加，j 在  [i, (i * i - 1)/2] 的范围找。

(define (a-pythagorean-triple-all)
  (let ((i (an-interger-start-fram 1)))
    (let ((j (an-integer-between i (/ (- (* i i) 1) 2))))
      (let ((ksq (+ (* i i) (* j j))))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))
