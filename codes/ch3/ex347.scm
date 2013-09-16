;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (semaphore-mutex n)
  (let ((cell n)
        (mutex (make-mutex)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (< n 1)
                 (the-mutex 'acquire)
                 (begin
                   (mtx 'acquire)
                   (set! n (- n 1))
                   (mtx 'release))))
            ((eq? m 'release)
             (begin
               (mtx 'acquire)
               (set! n (+ n 1))
               (mtx 'release)))))
    the-mutex))





(define (semaphore-test n)
  (let ((cell n))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release)
             (set! cell (+ 1 cell)))))
    the-mutex))

(define (test-and-set! n)
  (if (< n 1)
      #t
      (set! n (- n 1))))
