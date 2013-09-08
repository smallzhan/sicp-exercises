(define (reserve lists)
  (if (null? lists)
      lists
      (append (reserve (cdr lists)) (list (car lists)))))

(reserve (list 1 4 9 16 25))

(define hi (list 1 2 3 4))

(cons hi 5)
(cons 5 hi)
(list 5)