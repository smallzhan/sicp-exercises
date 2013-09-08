(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define l (make-branch 1 2))
(define r (make-branch 3 (list (list 1 2) (list 3 4))))

(define m (make-mobile l r))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(left-branch m)
(right-branch m)

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(branch-length (left-branch m))
(branch-structure (right-branch m))

(define (total-weight mobile)
  (let ((left-structure (branch-structure (left-branch mobile)))
	(right-structure (branch-structure (right-branch mobile))))
    (+ (branch-length (left-branch mobile))
       (branch-length (right-branch mobile))
       (if (pair? left-structure)
	   (total-weight left-structure)
	   0)
       (if (pair? right-structure)
	   (total-weight right-structure)
	   0))))

(total-weight m)

(define (blanced? mobile)
  (= (torque (left-branch mobile))
     (torque (right-branch mobile))))

(define (torque branch)
  (if (not (pair? (branch-structure branch)))
      (* (branch-length branch)
	 (branch-structure branch))
      (* (branch-length branch)
	 (+ (torque (left-branch (branch-structure branch)))
	    (torque (right-branch (branch-structure branch)))))))

(define m2 (list (list 1 4) (list 2 2)))
m2
(define m3 (list (list 1 2) (list 3 4)))
(blanced? m2)
(blanced? m3)
(blanced? m)

(cons (list 1 2) (list 3 4))

;; the d. part of the problem is easy to handle just substitute the `cadr' in
;; orignal function to `cdr' is OK!