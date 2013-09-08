(define (reserve lists)
  (if (not (pair? lists))
      lists
      (append (reserve (cdr lists)) (list (car lists)))))

(define (deep-reverse lists)
  (if (not (pair? lists))
      lists
      (append
       (deep-reverse (cdr lists))
       (list (deep-reverse (car lists))))))

(define x (list (list 1 2) (list 3 4 (list 5 6))))
x
(deep-reverse x)
(deep-reverse (list 1 2))
(reverse (list 3 4 (list 5 6)))
