;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;; a. 会把 (define x 3) 当做一个 application 去运行原来语言中本身有的 define，换句话说，
;; 如果赋值是个其他的关键字，去应用的时候会产生错误。

;; b. 需要修改的地方如下：

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp)
  (cadr exp))

(define (operands exp)
  (cddr exp))
