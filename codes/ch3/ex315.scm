;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x (list 'a 'b))
(define z1 (cons x x))

(define z1 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
(set-to-wow! z2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 不同在于， z1 的 car 和 cdr 指向同一个对象，因此对 car 的更改同时反映到了cdr
;; z2 的 car 和 cdr 仅仅时值共享，修改后，car 的那个值更新了，没影响 cdr
