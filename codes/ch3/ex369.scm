;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; using pairs.

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (triples-right? i j k)
  (= (+ (* i i) (* j j)) (* k k)))




(define the-triples (triples integers integers integers))

(define the-right-triples
  (stream-filter (lambda (triple)
                   (triples-right? (car triple) (cadr triple) (caddr triple)))
                 the-triples))

(stream-ref the-right-triples 0)
(stream-ref the-right-triples 1)
(stream-ref the-right-triples 2)
(stream-ref the-right-triples 3)
(stream-ref the-right-triples 4)
(stream-ref the-right-triples 5)
