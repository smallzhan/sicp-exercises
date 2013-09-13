;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(define (make-deque) (cons '() '()))
(define (front-deque deque) (car deque))
(define (rear-deque deque) (cdr deque))
(define (prev-node node) (car node))
(define (next-node node) (cddr node))
(define (value-node node) (cadr node))
(define (empty-deque? deque) (null? (front-deque deque)))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (set-next! node v) (set-cdr! (cdr node) v))



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
          ((eq? (front-deque deque) (rear-deque deque))
           (set-front-ptr! deque '())
           (set-rear-ptr! deque '())
           deque)
          (else
           (set-front-ptr! deque (next-node (front-deque deque)))
           (if (pair? (front-deque deque))
               (set-car! (front-deque deque) '())
               #t)
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
        ((eq? (front-deque deque) (rear-deque deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)
        (else
         (set-rear-ptr! deque (prev-node (rear-deque deque)))
         (if (pair? (rear-deque deque))
             (set-next! (rear-deque deque) '())
             #t)
         deque)))


(define (print-deque deque)
  (define (print-node node)
    (if (null? node)
        #t
        (begin
          (display (value-node node))
          (display " ")
          (print-node (next-node node)))))
  (begin
    (display "( ")
    (print-node (front-deque deque))
    (display ")")
    (newline)))



(define dq (make-deque))
(print-deque dq)
(front-insert-deque! dq 1)
(print-deque dq)
(front-insert-deque! dq 2)
(print-deque dq)
(rear-insert-deque! dq 3)
(print-deque dq)
(rear-delete-deque! dq)
(print-deque dq)
(front-delete-queue! dq)
(print-deque dq)
(front-delete-queue! dq)
(print-deque dq)
