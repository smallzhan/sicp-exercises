;;define a timer in a function

(define (make-monitored f)

(define timer 0)
;;(define (make-timer timer)

(define (dispatch mf)
  (cond ((eq? mf 'how) timer)
        ((eq? mf 'reset) (set! timer 0))
        (else (set! timer (+ timer 1))
	      (f mf))))
dispatch)
;(make-timer 0))

;;run
(define s (make-monitored sqrt))
(s 100)
(s 'how)
(s 'reset)
(s 'how)
