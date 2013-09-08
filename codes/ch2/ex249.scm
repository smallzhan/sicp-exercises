(define f (make-frame (cons 0 0) (cons 1 0) (cons 0 1)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


(define segment-list-a (list (cons (cons 0 0) (cons 1 0))
			     (cons (cons 1 0) (cons 1 1))
			     (cons (cons 1 1) (cons 0 1))
			     (cons (cons 0 1) (cons 0 0))))

(define segment-list-b (list (cons (cons 0 0) (cons 1 1))
			     (cons (cons 1 0) (cons 0 1))))

(define segment-list-c (list (cons (cons 0.5 0) (cons 1 0.5))
			     (cons (cons 1 0.5) (cons 0.5 1))
			     (cons (cons 0.5 1) (cons 0 0.5))
			     (cons (cons 0 0.5) (cons 0.5 0))))

(define segment-list-d (list ......))

(define a (list 1 2 3 4 5))

(for-each (lambda (a) (display (square a))) a)
