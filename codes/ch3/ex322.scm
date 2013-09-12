(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))

    (define (set-front-ptr! v)
      (set! front-ptr v))

    (define (insert-queue! v)
      (if (empty-queue?)
          (begin
            (set! front-ptr (list v))
            (set! rear-ptr front-ptr)
            )
          (begin
            (set-cdr! rear-ptr (list v))
            (set! rear-ptr (cdr rear-ptr)))
          ))
    (define (delete-queue!)
      (if (empty-queue?) (error? "DELETE! called with an empty queue")
          (set! front-ptr (cdr front-ptr))))
    (define (print)
      (display "Queue is ")
      (display front-ptr)
      (newline)
      )

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print)
            (else (error "Undefined operation -- MAKEQUEUE" m))))
    dispatch))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; test

(define z (make-queue))
((z 'insert-queue!) 3)
((z 'insert-queue!) 4)
((z 'delete-queue!))
((z 'print-queue))
