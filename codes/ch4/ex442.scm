;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
(define (tests-lier)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (distinct? betty ethel joan kitty mary))
    (require (not (equal? (= kitty 2) (= betty 3))))
    (require (not (equal? (= ethel 1) (= joan 2))))
    (require (not (equal? (= joan 3) (= ethel 5))))
    (require (not (equal? (= kitty 2) (= mary 4))))
    (require (not (equal? (= mary 4) (= betty 1))))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))
