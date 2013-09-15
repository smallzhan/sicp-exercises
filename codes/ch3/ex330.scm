;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 原始顺序比较难处理，因此假定输入信号的列表是低位在前，高位在后

(define (ripple-carry-adder a b s c)
  (let ((ain (make-wire))
        (bin (make-wire))
        (cin (make-wire))
        (cout (make-wire))
        (sout (make-wire)))
    (define (carry-adder-iter a b c)
      (if (null? a)
          (set! c (get-signal cout))
          (begin
            (set-signal! ain (car a))
            (set-signal! bin (car b))
            (set-signal! cin c)
            (set-signal! cout 0)
            (set-signal! sout 0)
            (full-adder ain bin cin sout cout)
            (set! s (cons (get-signal sout) s))
            (carry-adder-iter (cdr a) (cdr b) (get-signal cout)))))
    (if (not (= (length a) (length b)))
      (error "The input of the RIPPLE-CARRY-ADDER is not compatible")
      (carry-adder-iter a b 0))))
