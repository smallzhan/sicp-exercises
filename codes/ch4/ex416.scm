;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned value" (car vals))
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame))))))

;;;;b
(define (scan-out-defines proc)
  (let ((vars (proc-vars proc))
        (defs (proc-defs proc))
        (body (proc-body proc)))
    (make-lambda
     vars
     (list (make-out-def-let defs body)))))


(define (proc-vars proc)
  (cadr proc))

(define (proc-defs proc)
  (define (is-def? pair)
    (and (pair? pair) (eq? (car pair) 'define)))
  (filter is-def? proc))

(define (proc-body proc)
  (if (null? (cdr proc))
      (car proc)
      (proc-body (cdr proc))))

(define (make-out-def-let defs body)
  (define (def->set! def)
    (cons 'set! (cdr def)))
  (define (def-unassigned def)
    (cons (cadr def) (list '*unassigned*)))
  (let ((set!-body (map def->set! defs))
        (let-vars (map def-unassigned defs)))
    (cons 'let (cons let-vars (append set!-body (list body))))))

;;;; b.test
(define test-lambda-exp '(lambda (a b c)
                           (define u (a 1))
                           (define v (b 2))
                           (c 3)))
(scan-out-defines test-lambda-exp)

;;;;c. 放在 body 里面，这样调用的时候才完成转换，不调用不转换
