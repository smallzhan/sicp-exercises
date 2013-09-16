;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Louis 不对。采用复杂精细的方法是没有必要的。
;;;;;;;;;;;;;;;; transfer 保证 withdraw 和 deposite 的是同样的值。
;;;;;;;;;;;;;;;; 只要这两个过程被很好的串行化了，就不会影响最终的结果。
;;;;;;;;;;;;;;;; 与 exchange 不同的是，那里有计算 difference 的一步
;;;;;;;;;;;;;;;; 那一步需要两次读操作，如果不串行化，会计算出错误的 diff 值。