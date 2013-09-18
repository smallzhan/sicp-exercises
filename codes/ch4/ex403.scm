;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (install-eval-package)
  (define (assignment- exp env)
    (eval-assignment exp env))
  (define (define- exp env)
    (eval-define exp env))
  (define (if- exp env)
    (eval-if exp env))
  (define (lambda- exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)))
  (define (begin- exp env)
    (eval-sequence (begin-actions exp) env))
  (define (cond- exp env)
    (eval (cond->if exp) env))
  (define (application- exp env)
    (apply (eval (operator exp) env)
           (list-of-values (operands exp) env)))

  (put 'eval 'define define-)
  (put 'eval 'set! assignment-)
  (put 'eval 'if if-)
  (put 'eval 'lambda lambda-)
  (put 'eval 'begin begin-)
  (put 'eval 'cond cond-)
  (put 'eval 'call application-) ;; this is ugly.
  done)


(define (eval exp env)
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))
        ((string? exp) exp)
        ((get 'eval (typetag exp))
         ((get 'eval (typetag exp)) exp env))

        ((application? exp)
         (apply (eval (operator exp) env)
          (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (typetag exp)
  (if (pair? exp)
      (car exp)))
