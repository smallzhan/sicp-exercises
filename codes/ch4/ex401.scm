;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (list-of-values-l exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (let ((right (list-of-values-l (rest-operands exps) env)))
          (cons left right)))))


(define (list-of-values-r exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-r (rest-operands exps) env)))
        (let ((left (eval (first-operand exps) env)))
          (cons left right)))))
