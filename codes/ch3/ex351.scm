;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
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



;; stream-car and stream-cdr would normally be built into
;;  the stream implementation
                                        ;: (define (stream-car stream) (car stream))
                                        ;: (define (stream-cdr stream) (force (cdr stream)))

                                        ;: (stream-car
                                        ;:  (stream-cdr
                                        ;:   (stream-filter prime?
                                        ;:                  (stream-enumerate-interval 10000 1000000))))

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

(define (show x)
  (display-line x)
  x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x (stream-map show (stream-enumerate-interval 0 10))) ;; 应该是0, 因为只有第一个被求值了。

(stream-ref x 5) ;; 前 5 个。 1 -- 5. 会有 5 个被 show 函数调用

(stream-ref x 7) ;; 我以为是 1--7, 但是结果是  6, 7
;; 是 memo-proc 在搞鬼！！ 访问的时候不会直接 show 一次了。
;; 而是直接使用记忆值。
