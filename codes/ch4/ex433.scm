;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
        (eval# (lazy->list text) env)
        text)))

(define (lazy-cons x y)
  (cons 'cons (cons (cons 'quote (list x)) (list y))))

(define (lazy->list text)
  (if (null? text)
      '()
      (lazy-cons (car text)
                 (lazy->list (cdr text)))))
