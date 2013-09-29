;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define one (cons-stream 1 ones)) ;;;; 会生成流 (1 1 1 .....)
(set! THE-ASSERTIONS (cons-stream assertion THE-ASSERTIONS)) ;;;;也是循环定义。
;;;; THE-ASSERTIONS 不会被求值，会指向自己
