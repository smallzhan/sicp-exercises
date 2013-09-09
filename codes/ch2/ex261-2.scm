;; ex2.61 and ex2.62 ORDERED
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


(define (adjoin-set x set)
  (define (search-adjoin head tail)
    (cond ((null? tail)
           (if (null? head)
               (list x)
               (append head (list x))))
          ((= x (car tail)) set)
          ((< x (car tail))
           (append head (list x) tail))
          (else
           (if (null? head)
               (search-adjoin (list (car tail)) (cdr tail))
               (search-adjoin (append head (list (car tail)))
                              (cdr tail))))))
  (search-adjoin '() set))


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2)))))))))



(define set1 (adjoin-set 1 (adjoin-set 5 (adjoin-set 4 (adjoin-set 9 (adjoin-set 2 '()))))))
;;(1 2 4 5 9)
(define set2 '(2 3 4 6 7))
(intersection-set set1 set2)
;;(2 4)
(union-set set1 set2)
;;(1 2 3 4 5 6 7 9)
