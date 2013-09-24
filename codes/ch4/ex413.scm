;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; 这里的处理方式是删除求值的那个环境中的约束，如果求值的环境中没有，那么
;;;; 一直王后找，直到找到为止。
;;;; 其实采用 4.11 的那种表示方法最简单了, 不过还是先用这个书上的吧

(define (make-unbind! var env)
  (if (eq? env the-empty-environment)
      (error "Unbind variable" var)
      (if (unbind-var! var env)
          #t
          (make-unbind! var (enclosing-environment env)))))

(define (unbind-var! var env)
  (display env)
  (newline)
  (let ((frame (first-frame env))
        (val (lookup-in-frame var env)))
    (if val
        (begin
          (set-car! frame (filter (lambda (x) (not (eq? x var))) (frame-variables frame)))
          (set-cdr! frame (filter (lambda (x) (not (eq? x (car val)))) (frame-values frame)))
          #t)
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define initial-env
  (extend-environment '(a b c) '(1 2 3) the-empty-environment))

;; (lookup-variable-value 'a initial-env)

;; (set-variable-value! 'a 3 initial-env)
;; (define-variable! 'd 4 initial-env)
;; (lookup-variable-value 'd initial-env)
;; (lookup-variable-value 'e initial-env)

(define extend-env
  (extend-environment '(h i j) '(10 11 12) initial-env))

;; (lookup-variable-value 'a extend-env)
;; (lookup-variable-value 'e extend-env)
(make-unbind! 'e initial-env)
(make-unbind! 'a extend-env)

(define-variable! 'b 9 extend-env)
(make-unbind! 'b extend-env)
