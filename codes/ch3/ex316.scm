;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (count-pairs x)
  (display x)
  (newline)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(count-pairs '(() () ())) ;;; 3
(count-pairs '(() () (a))) ;;; 4
(count-pairs '((a) (b) (c))) ;;; 6
(count-pairs '((a) (b c) (d))) ;;; 7

(define x '(a))
(make-cycle x)
(define y (cons x (cons '(b) '((c)))))
;;y ((a . #0#) (b) (c))

(count-pairs y) ;;; no result, stackoverflow
