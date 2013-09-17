;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some libraries

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20))) ;;(1)
(define y (stream-filter even? seq)) ;;(2)
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))          ;; (3)

(stream-ref y 7)                        ;; (4)
(display-stream z)                      ;; (5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; seq = (1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 66, 78, 91, 105, 120, 136, 153, 171, 190, 210)
;; (1). sum = 1
;; (2). sum = 6, the third of seq
;; (3). sum = 10, the fourth of seq
;; (4). sum = 136, the seventh of seq
;; (5). 10 15 45....
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 不使用 memo-proc，结果会大不一样。累加次数增多
;; (1). sum = 1, 没问题, 理论上序列为 (1, promise of (accum (stream-enumerate-interval 2, 20)))
;; (2). 访问 1 时 sum +1, 访问第二个数，本应是2,此时 + sum ，得到 4, 是偶数，结束。 (1, 2, promise ...(3, 20))
;; (3). sum = 4, 访问 1 2 得到 sum = 9, 继续 accum， 第三个数是 3， +9 =12, seq =(1, 2, 12, ...)
;;      第四个数是 4, 访问后得到 16, 第 5 个数是 5, 访问后得到 21 然后依次是 27 34 42 51 62 74 87 101 116 ...
;; (4). ...
;; (5). ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 流不加上 memo-proc 对于有赋值的操作来说简直就是灾难啊。。。
