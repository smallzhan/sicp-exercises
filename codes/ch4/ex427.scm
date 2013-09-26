;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define count 0)

(define (id x)
  (set! count (+ count 1)) x)

(define w (id (id 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
count
;; count = 1, w = (trunks (id 10) <env>)--- delayed

w
;; w = (force-it (id 10)) = 10
;; count = 2

count
;; count = 2
