;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


;;;;;;;;;;;;;;;; a.
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))

;;;; (proc (car items)) 是函数应用，在 apply 的时候 proc 会使用 actual-value 求值 到 lambda
;;;; (car items) 被变成 trunk, 但是再次被 display 强迫。
;;;; 因此副作用都在。

;;;;;;;;;;;;;;;; b.
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
   e
   x)
  (p (set! x (cons x '(2)))))

(p1 1) ;; 都是 （1 2）
(p2 1) ;; 采用正文中的方法是 1, Cy 的方法是 (1 2)

;;;;;;;;;;;;;;;; c.
;;;; 对 a 中的 for-each 不会影响，因为在 eval-sequence 整个进行 actual-value 求值了。
;;;; 在 eval proc 的过程中，虽然还要对 proc 求 actual-value， 但是此时已经是 actual-value 了，
;;;; 所以没关系。

;;;;;;;;;;;;;;;; d.
;;;; Cy 的方案应该更加保险。
