;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; (for (for-args) (for-conds) for-body) ;; initial version

(define (for? exp) (tagged-list? 'for))
(define (for-args exp) (cadr exp))
(define (for-condition exp) (caddr exp))
(define (for-body exp) (cadddr exp))
(define (for-initial condition)
  (car condition))
(define (for-test condition)
  (cadr condition))
(define (for-increment condition)
  (caddr condition))

(define (for->func exp)
  (make-begin
   (list
    (make-define (cons 'for-loop (for-args exp))
                 (make-if (for-test (for-condition exp))
                          (make-begin
						   (list (for-body exp)
								 (cons 'for-loop (for-increment (for-condition exp)))))
						  ''ok
                          ))
    (cons 'for-loop (map cadr (for-initial (for-condition exp)))))))

(define (make-define funcname body)
  (cons 'define (list funcname body)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (make-begin seq)
  (cons 'begin seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; test
(define c 0)
(define for-exp '(for
                     (a b)
                   (((a 1) (b 2)) (< a 10) ((+ a 1) (+ b 2)))
                   (set! c (+ c a))))

(for->func for-exp)

(for-args for-exp)
(for-condition for-exp)
(for-body for-exp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; the translated code is

(begin
  (define (for-loop a b)
	(if (< a 10)
		(begin (set! c (+ c a))
			   (for-loop (+ a 1) (+ b 2)))
		'ok))
  (for-loop 1 2))
