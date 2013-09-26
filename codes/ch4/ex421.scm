;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Y combinator
;;;;a

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)

;;;;1: n = 10 application
((lambda (fact)
   (fact fact 10)) ;;;; <HERE>
 (lambda (ft k)
   (if (= k 1)
       1
       (* k (ft ft (- k 1))))))

;;;; 2. <HERE> the first fact = (lambda (ft k) ...., the second fact = ft k = 10
(* 10 (fact fact 9))
;;;; ...
(* 10 (* 9 (fact fact 8)))
....
(* 10 (* 9 (* 8 ..... (* 2 (fact fact 1)))))
(* 10 (* 9 (* 8.... (* 2 1))))


;;;;b
(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

;;; 完全搞不懂，填空还是会一点。。。
