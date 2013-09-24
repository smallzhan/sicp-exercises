;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (lookup-in-frame var env)
  (define (scan vars vals)
    (cond ((null? vars) #f)
          ((eq? var (car vars)) vals)
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      #f
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))


;; (define (lookup-in-env var env)
;;   (define (env-loop env)
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (env-loop (enclosing-environment env)))
;;             ((eq? var (car vars))
;;              (car vals))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (if (eq? env the-empty-environment)
;;         #f
;;         (let ((frame (first-frame env)))
;;           (scan (frame-variables frame)
;;                 (frame-values frame))))))

(define (lookup-in-env var env)
  (let ((first-res (lookup-in-frame var env)))
    (if first-res
        first-res
        (lookup-in-frame var (enclosing-environment env)))))


(define (lookup-variable-value var env)
  (let ((res (lookup-in-env var env)))
    (if res
        (car res)
        (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((res (lookup-in-env var env)))
    (if res
        (set-car! res val)
        (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((res (lookup-in-frame var env)))
    (if res
        (set-car! res val)
        (add-binding-to-frame! var val (first-frame env)))))


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

(lookup-variable-value 'a initial-env)

(set-variable-value! 'a 3 initial-env)
(define-variable! 'd 4 initial-env)
(lookup-variable-value 'd initial-env)
(lookup-variable-value 'e initial-env)

(define extend-env
  (extend-environment '(h i j) '(10 11 12) initial-env))

(lookup-variable-value 'a extend-env)
(lookup-variable-value 'e extend-env)
