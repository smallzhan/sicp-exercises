;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4)))
    (let ((cooper (amb 2 3 4 5)))
      (let ((flether (amb 2 3 4)))
        (require (not (= (abs (- cooper flether)) 1)))
        (let ((miller (amb 3 4 5))) ;; since miller > cooper, and cooper >= 2
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= (abs (smith flether) 1))))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'flether flether)
                  (list 'miller miller)
                  (list 'smith smith))))))))
