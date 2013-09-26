;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((choice (random-choice choices)))
            (choice
             env
             succeed
             (lambda ()
               (try-next (rest-choices choice choices)))))))
    (try-next cprocs))))


(define (random-choice choices)
  (let ((i (random (length choices))))
    (list-ref choices i)))

(define (rest-choices choice choices)
  (filter (lambda (x) (not (eq? choice x))) choices))
