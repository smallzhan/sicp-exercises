;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (make-deque) (cons '() '()))
(define (front-deque deque) (car deque))
(define (rear-deque deque) (cdr deque))
(define (prev-node node) (car node))
(define (next-node node) (cddr node))
(define (empty-deque? deque) (null? (front-deque deque)))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (set-next! node v)
  (set-cdr! (cdr node) v))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (set-cdr! (rear-deque queue) new-pair)
    (set-rear-ptr! queue new-pair)
    queue))

(define (delete-queue! queue)
  (cond ((empty-deque? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))



(define (rear-insert-deque! deque item)
  (let ((new-node (cons '() (cons item '()))))
   (cond ((empty-deque? deque)
          (set-front-ptr! deque new-node)
          (set-rear-ptr! deque new-node)
          deque)
         (else
          (let ((prev-rear (rear-deque deque)))
            (set-next! prev-rear new-node)
            (set-rear-ptr! deque new-node)
            (set-car! new-node prev-rear)
          deque)))))

(define (front-delete-queue! deque)
    (cond ((empty-deque? deque)
           (error "DELETE! called with an empty queue" queue))
          (else
           (set-front-ptr! deque (cddr (front-deque deque)))
           (set-car! (front-deque deque) '())
           deque)))


(define (front-insert-deque! deque item)
  (let ((new-node (cons '() (cons item '()))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (let ((prev-front (front-deque deque)))
             (set-next! new-node prev-front)
             (set-front-ptr! deque new-node)
             (set-car! prev-front new-node)
           deque)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-rear-ptr! deque (car (rear-deque deque)))
         (set-next! (rear-deque deque) '())
         deque)))


(define (print-deque deque)
  (define (print-node node)
    (if (null? node)
        #t
        (begin
          (display (cadr node))
          (display " ")
          (print-node (cddr node)))))
  (begin
    (display "( ")
    (print-node (front-deque deque))
    (display ")")
    (newline)))
