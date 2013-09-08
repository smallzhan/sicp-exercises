;;as exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count) (fib-iter a
                                 b
                                 (+ (square p) (square q))
                                 (+ (* 2 p q) (square q))
                                 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (square x)
  (* x x))

(define (fibo n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibo (- n 1)) (fibo (- n 2))))))

(define (fibn n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (fibn-iter 1 0 (- n 1)))))

(define (fibn-iter a b count)
  (if (= count 0)
      a
      (fibn-iter (+ a b) a (- count 1))))
