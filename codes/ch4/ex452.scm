;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-if exp)
  (cadr exp))
(define (if-fail-fail exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (let ((if-part (analyze (if-fail-if exp)))
        (fail-part (analyze (if-fail-fail exp))))
    (lambda (env succeed fail)
      (if-part env
               (lambda (if-value fail2)
                 (if (true? if-value)
                     (succeed if-value fail2)
                     (fail-part env succeed fail2)))
               fail))))
