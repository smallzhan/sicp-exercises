
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))

(define empty-board ())

(define (safe? k positions)
  (if (= k 1)
      #t
      (let ((target (car positions))
            (source (cadr positions)))
        (and (not (= (cadr target) (cadr source)))
             (not (= (abs (- (cadr target) (cadr source)))
                     (- (car target) (car  source))))
             (safe? (- k 1) (cons (car positions) (cddr positions)))))))

;; 下面是在一个网站上发现的版本，个人觉得比我的写得优秀一些，存照留念
;; (define (safe? k positions)
;;   (define (check x y)
;;     (or (= (cadr x) (cadr y))
;;         (= (abs (- (cadr x) (cadr y)))
;;            (abs (- (car x) (car y))))))
;;   (let ((p (car positions))
;;         (l (cdr positions)))
;;     (or (= k 1) (null? (filter (lambda (x) (check x p)) l)))))

(queens 8)
(define rest ())
(cons (list 3 4) (cons (list 1 2) (list rest)))

;;(map (lambda (x) (list x (* 2 x))) (list 1 2 3 4))

(flatmap square (list 1 2 3 4 5 6))
