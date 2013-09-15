;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define balance 100)
(set! balance (+ balance 10))           ;a
(set! balance (- balance 20))           ;b
(set! balance (- balance (/ balance 2))) ;c

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 顺序执行:
;; a->b->c: 45
;; a->c->b: 35
;; b->a->c: 45
;; b->c->a: 50
;; c->a->b: 40
;; c->b->a: 40
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 交错执行  110 80 50....
;;;; a: 读 写 a1:a2
;;;; b: 读 写 b1:b2
;;;; c: 读 写 c1:c2:c2 (读了两次！)
;;;; 一共有 7!/(2!*2!*3!) = 210 种可能的排列