(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-2 origin edge1 egde2)
  (cons origin (cons edge1 edge2)))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame-2 frame)
  (edge1-frame frame))

(define (edge2-frame-2 frame)
  (cddr frame))

(define origin (make-vect 1 1))
(define edge1 (make-vect 1 2))
(define edge2 (make-vect 2 1))

(define f (make-frame-2 origin edge1 edge2))
(edge1-frame-2 f)
(origin-frame f)
(edge2-frame-2 f)