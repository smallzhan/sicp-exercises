(define (cycle? x)
  (define (cycle-set x set)
    (if (not (pair? x))
        #f
      (if (memeq? x set)
          #t
          (cycle-set (cdr x) (cons x set)))))
  (cycle-set x '()))


(define (memeq? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else
         (memeq? x (cdr set)))))
